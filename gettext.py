#!/usr/bin/env python3

import re
import sys
import string

WORD_CHARS = frozenset(string.ascii_letters + string.digits)

# TODO: support new C++ custom literals
LEX = re.compile(
    r'((?i:(?:[LuU]|u8)?"(?:[^"\n\\]|\\[?"'+"'"+r'rnabfvt]|\\[0-9]{1,3}|\\x[0-9a-fA-F]{2}|\\U[0-9a-fA-F]{8}|\\u[0-9a-fA-F]{4})*"|R"[^\n"]*"))|' + # string
    r"((?i:(?:[LuU]|u8)?'(?:[^'\n\\]|\\[?'"+'"'+r"rnabfvt]|\\[0-9]{1,3}|\\x[0-9a-fA-F]{2}|\\U[0-9a-fA-F]{8}|\\u[0-9a-fA-F]{4})*'))|" + # char (yes they can be more than on character in C)
    r'([_a-z][_a-z0-9]*)|' +        # identifier
    r'(/\*(?:[^*]|\*[^/])*\*/)|' +  # multiline comment
    r'(//[^\n]*)|' +                # single line comment
    r'([ \v\t\r\n]+)|' +            # space
    r'([-+]?[0-9]+(?:\.[0-9]*)?(?:e[-+]?[0-9]+)?i?|[-+]?0x[0-9a-f]+|[-+]?[0-9]*\.[0-9]+(?:e[-+]?[0-9]+)?i?)|' + # numbers
    r'(@[_a-z][_a-z0-9]*)|' + # tag
    r'([-+*/%&|^!=]=|<<|>>|\|\||&&|->|\.\.\.|--|\+\+|##|::|[-+./*,!^~<>|&?:%=;])|' + # operators
    r'([{}()\[\]])|' + # brackets
    r'(#(?:\\\n|[^\n])*)', # preproc
    re.M | re.I)

STRING = 1
CHAR   = 2
IDENT  = 3
ML_COMMENT = 4
COMMENT = 5
SPACE = 6
NUMBER = 7
TAG = 8
OPERATOR = 9
BRACKET = 10
PREPROC = 11

TREE = -1
ILLEGAL = -2

TOKENS = (IDENT, STRING, CHAR, ML_COMMENT, COMMENT, SPACE, OPERATOR, BRACKET, NUMBER, TAG, PREPROC)

STR_PART = re.compile(r'([^"\n\\])|\\([?"'+"'"+r'rnabfvt])|\\([0-9]{1,3})|\\x([0-9a-fA-F]{2})|\\U([0-9a-fA-F]{8})|\\u([0-9a-fA-F]{4})')
RAW_STR  = re.compile(r'^R"([^"]*)"$')

ESC_SIMPLE = {
    "?": "?",
    "'": "'",
    '"': '"',
    'r': '\r',
    'n': '\n',
    'a': '\a',
    'b': '\b',
    'f': '\f',
    'v': '\v',
    't': '\t',
}

def _parse_string_repl(m):
    if m.group(1):
        return m.group(1)
    elif m.group(2):
        return ESC_SIMPLE[m.group(2)]
    elif m.group(3):
        return chr(int(m.group(3), 8))
    elif m.group(4):
        return chr(int(m.group(4), 16))
    elif m.group(5):
        return chr(int(m.group(5), 16))
    elif m.group(6):
        return chr(int(m.group(6), 16))
    assert(False)

def parse_string(val):
    if not val.endswith('"'):
        # TODO: C++ custom string literals
        raise SyntaxError(val)

    if val.startswith('u8"'):
        prefix = 'u8'
        s = val[3:-1]
    elif val.startswith('u"'):
        prefix = 'u'
        s = val[2:-1]
    elif val.startswith('U"'):
        prefix = 'U'
        s = val[2:-1]
    elif val.startswith('L"'):
        prefix = 'L'
        s = val[2:-1]
    elif val.startswith('R"'):
        return 'R', RAW_STR.match(val).group(1)
    elif val.startswith('"'):
        prefix = ''
        s = val[1:-1]
    else:
        raise SyntaxError(val)
    
    return prefix, STR_PART.sub(_parse_string_repl, s)

def _prefix_msg(prefix):
    return "string literal prefix " + prefix if prefix else "no string literal prefix"

def parse_strings(tokens):
    # NOTE: this accepts concatenation of incompatible string literlas, like: L"foo" "bar"
    buf = []
    prefix = None
    for tok in tokens:
        if tok.tok != STRING:
            raise SyntaxError("not a string token: %s" + tok)
        p, val = parse_string(tok.val)
        if prefix is None:
            prefix = p
        elif p != prefix:
            raise ParserError(tok.lineno, tok.column, tok.start, tok.end, _prefix_msg(prefix), _prefix_msg(p))
        buf.append(val)
    return ''.join(buf)

def tokens(s):
    index = 0
    n = len(s)
    lineno = 1
    column = 1
    while index < n:
        m = LEX.match(s, index)

        if not m:
            raise ParserError(lineno, column, index, index + 1, 'a valid token', s[index:index + 1])

        for TOK in TOKENS:
            val = m.group(TOK)
            if val is not None:
                yield (m.start(), m.end(), lineno, column, TOK, val)
                break
 
        val = m.group(0)
        newlines = val.count("\n")
        if newlines > 0:
            lineno += newlines
            column = len(val) - val.rfind("\n")
        else:
            column += len(val)
        
        index = m.end()

class Node:
    __slots__ = ()

    @property
    def tok(self):
        raise NotImplementedError

    @property
    def start(self):
        raise NotImplementedError

    @property
    def end(self):
        raise NotImplementedError

    @property
    def lineno(self):
        raise NotImplementedError

    @property
    def column(self):
        raise NotImplementedError

class Atom(Node):
    __slots__ = 'start', 'end', 'lineno', 'column', 'tok', 'val'

    def __init__(self, start, end, lineno, column, tok, val):
        self.start = start
        self.end = end
        self.lineno = lineno
        self.column = column
        self.tok = tok
        self.val = val

    def __str__(self):
        return self.val

class Tree(Node):
    __slots__ = 'tokens', 'tok'

    def __init__(self):
        self.tokens = []
        self.tok = TREE

    def __str__(self):
        return join_tokens(self.tokens)

    @property
    def start(self):
        return self.tokens[0].start

    @property
    def end(self):
        return self.tokens[-1].end

    @property
    def lineno(self):
        return self.tokens[0].lineno

    @property
    def column(self):
        return self.tokens[0].column

    def is_strings(self):
        for tok in self.tokens:
            if tok.tok != STRING:
                return False
        return True

class ParserError(Exception):
    def __init__(self, lineno, column, start, end, expected, got):
        Exception.__init__(self, "expected %s, but got %s" % (expected, got))
        self.lineno = lineno
        self.column = column
        self.start = start
        self.end = end
        self.expected = expected
        self.got = got

def parse(s):
    node = Tree()
    stack = []
    for item in tokens(s):
        tok = item[4]
        if tok in (ML_COMMENT, COMMENT, SPACE):
            continue

        if tok == BRACKET:
            val = item[5]
            if val in "[({":
                stack.append(node)
                child = Tree()
                child.tokens.append(Atom(*item))
                node.tokens.append(child)
                node = child
            else:
                child = Atom(*item)
                node.tokens.append(child)
                start = node.tokens[0].val
                if start == '(':
                    if child.val != ')':
                        raise ParserError(child.lineno, child.column, child.start, child.end, ')', child.val)
                elif start == '{':
                    if child.val != '}':
                        raise ParserError(child.lineno, child.column, child.start, child.end, '}', child.val)
                elif start == '[':
                    if child.val != ']':
                        raise ParserError(child.lineno, child.column, child.start, child.end, ']', child.val)

                node = stack.pop()
        else:
            node.tokens.append(Atom(*item))

    return node

def parse_comma_list(tokens):
    parsed = []
    node = Tree()
    for tok in tokens:
        if tok.tok == OPERATOR and tok.val == ',':
            parsed.append(node)
            node = Tree()
        else:
            node.tokens.append(tok)

    if node.tokens:
        parsed.append(node)

    # FIXME: this allows trailing commas

    return parsed

def join_tokens(tokens):
    buf = [str(tok) for tok in tokens]
    i = 1
    while i < len(buf):
        c1 = buf[i - 1][-1]
        c2 = buf[i][0]
        if (c1 in WORD_CHARS and c2 in WORD_CHARS) or (c1 in "'\"" and c2 in "'\""):
            buf.insert(i, ' ')
            i += 1
        i += 1
    return ''.join(buf)

def print_gettext(s, func_name='_'):
    try:
        for ident_tok, args_tok in gettext(s, func_name):
            print('gettext at line %d column %d: _%s' % (ident_tok.lineno, ident_tok.column,
                join_tokens(args_tok.tokens)))

            args = parse_comma_list(args_tok.tokens[1:-1])
            if not args:
                print("\tNO ARGUMENTS!")
            else:
                arg0 = args[0]
                if arg0.is_strings():
                    try:
                        arg0 = parse_strings(arg0.tokens)
                    except SyntaxError as e:
                        raise ParserError(arg0.lineno, arg0.column, arg0.start, arg0.end, "string literal", s[arg0.start:arg0.end])
                    else:
                        print("\tparsed format argument: %r" % arg0)
                for argind, arg in enumerate(args):
                    print("\targ %d: %s" % (argind, arg))
            print()
    except ParserError as e:
        illegal = Atom(e.start, e.end, e.lineno, e.column, ILLEGAL, s[e.start:e.end])
        print_error(filename, s, illegal, illegal, str(e))

RED = "\x1b[31m"
BLUE = "\x1b[34m"
NORMAL = "\x1b[0m"

def print_error(filename, s, start_tok, end_tok, message):
    lines = s.split('\n')[start_tok.lineno - 1:end_tok.lineno]
    if sys.stdout.isatty():
        red = RED
        blue = BLUE
        normal = NORMAL
    else:
        red = blue = normal = ""
    line_padd = 1 + len(str(end_tok.lineno + s.count('\n', end_tok.start, end_tok.end)))
    print("%s:%d:%d" % (filename, start_tok.lineno, start_tok.column), message)
    for i, line in enumerate(lines):
        if not line.strip():
            continue
        start = 0 if i > 0 else start_tok.column - 1
        
        if i + 1 < len(lines):
            end = len(line)
        else:
            line_start = s.rfind('\n', 0, end_tok.end) + 1
            end = end_tok.end - line_start

        mark_len = end - start
        slineno = str(i + start_tok.lineno)
        print('%s%s%s |%s %s' % (blue, ' ' * (line_padd - len(slineno)), slineno, normal, line.replace('\t', '    ')))
        buf = ['%s%s |%s ' % (blue, ' ' * line_padd, normal)]
        for i in range(0, start):
            if line[i] == '\t':
                buf.append('    ')
            else:
                buf.append(' ')
        buf.append(red)
        for i in range(start, start + max(mark_len, 1)):
            if line[i] == '\t':
                buf.append('^^^^')
            else:
                buf.append('^')
        buf.append(normal)
        print(''.join(buf))
    print()

# only to be used on a comma list in parenthesis
def find_before_comma(tokens):
    for i, tok in enumerate(tokens):
        if tok.tok == OPERATOR and tok.val == ',':
            return tokens[i - 1]
    if tokens[-1].tok == BRACKET and tokens[-1].val == ')':
        return tokens[len(tokens) - 2]
    return tokens[len(tokens) - 1]

def validate_gettext(s, filename, func_name, valid_keys):
    try:
        for ident_tok, args_tok in gettext(s, func_name):
            args = parse_comma_list(args_tok.tokens[1:-1])
            if not args:
                print_error(filename, s, ident_tok, args_tok, "no arguments")
            else:
                arg0 = args[0]
                if arg0.is_strings():
                    try:
                        arg0 = parse_strings(arg0.tokens)
                    except SyntaxError as e:
                        raise ParserError(arg0.lineno, arg0.column, arg0.start, arg0.end, "string literal", s[arg0.start:arg0.end])
                    if arg0 not in valid_keys:
                        print_error(filename, s, args[0], find_before_comma(args_tok.tokens), "not a know string reference")
                else:
                    print_error(filename, s, args[0], find_before_comma(args_tok.tokens), "not a string literal")
    except ParserError as e:
        illegal = Atom(e.start, e.end, e.lineno, e.column, ILLEGAL, s[e.start:e.end])
        print_error(filename, s, illegal, illegal, str(e))

def gettext(s, func_name='_'):
    node = parse(s)
    yield from _gettext(node, func_name)

def _gettext(node, func_name):
    i = 0
    while i < len(node.tokens):
        child = node.tokens[i]
        if child.tok == IDENT:
            # NOTE: This can distinguish between _() and foo->_() / foo._() / foo::_(),
            #       but it cannot detect if _ is a local variable, since that would
            #       need a full blown Objective-C++ parser.
            if i > 0:
                prev = node.tokens[i - 1]
                if prev.tok == OPERATOR:
                    if prev.val in ('->', '.'):
                        i += 1
                        continue
                    
                    if i - 1 > 0 and prev.val == '::':
                        prev = node.tokens[i - 2]
                        if prev.tok == IDENT:
                            i += 1
                            continue

            if child.val == func_name and i + 1 < len(node.tokens) and node.tokens[i + 1].tok == TREE:
                next_child = node.tokens[i + 1]
                if next_child.tokens[0].val == '(':
                    yield child, next_child
                    i += 1
                
        elif child.tok == TREE:
            yield from _gettext(child, func_name)
        i += 1

if __name__ == '__main__':
    with open(sys.argv[1]) as fp:
        known = set(fp)
    if '' in known:
        known.remove('')
    filename = sys.argv[2]
    with open(filename) as fp:
        s = fp.read()
    #print_gettext(s, '_')
    validate_gettext(s, filename, '_', known)
