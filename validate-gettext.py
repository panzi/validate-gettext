#!/usr/bin/env python3

import re
import sys
import string

WORD_CHARS = frozenset(string.ascii_letters + string.digits)

CLOSE_BRACKET_MAP = {
    "(": ")",
    "[": "]",
    "{": "}"
}

OPEN_BRACKETS  = frozenset(CLOSE_BRACKET_MAP.keys())
CLOSE_BRACKETS = frozenset(CLOSE_BRACKET_MAP.values())

# XXX: This doesn't correctly parse parameters that use C++ templates with
#      multiple template parameters. See:
#
#      _("foo bar baz", std::pair<int, int>(1, 2), 123)
#        ^- 1st arg     ^- 2nd arg     ^- 3rd arg  ^- 4th arg
#
#      However, if its enclosed in paranthesis it works:
#
#      _("foo bar baz", (std::pair<int, int>(1, 2)), 123)
#        ^- 1st arg     ^- 2nd arg                   ^- 3rd arg
#
#      A full blown C++ parser would need to have type informations in order
#      to _parse_ such code. I won't write a full blown C++ parser.

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
    r'([-+*/%&|^!=<>]=|<<=?|>>=?|\|\||&&|->|\.\.\.|--|\+\+|##|::|[-+./*,!^~<>|&?:%=;])|' + # operators
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
        raise SyntaxError('illegal string literal: ' + val)

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
        raise SyntaxError('illegal string literal: ' + val)

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
            raise UnexpectedTokenError(tok.lineno, tok.column, tok.start, tok.end, _prefix_msg(prefix), _prefix_msg(p))
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
            raise UnexpectedTokenError(lineno, column, index, index + 1, 'a valid token', s[index:index + 1])

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
    def __init__(self, lineno, column, start, end, message):
        Exception.__init__(self, message)
        self.lineno = lineno
        self.column = column
        self.start  = start
        self.end    = end

class UnexpectedTokenError(ParserError):
    def __init__(self, lineno, column, start, end, expected, got):
        ParserError.__init__(self, lineno, column, start, end,
            "expected %s, but got %s" % (expected, got))
        self.expected = expected
        self.got = got

class UnbalancedParenthesisError(ParserError):
    def __init__(self, lineno, column, start, end,
            other_lineno, other_column, other_start, other_end,
            expected, got):
        ParserError.__init__(self, lineno, column, start, end,
            "expected %s, but got %s" % (expected, got))
        self.other_lineno = other_lineno
        self.other_column = other_column
        self.other_start  = other_start
        self.other_end    = other_end

def parse(s):
    node = Tree()
    stack = []
    for item in tokens(s):
        tok = item[4]
        if tok in (ML_COMMENT, COMMENT, SPACE):
            continue

        if tok == BRACKET:
            val = item[5]
            if val in OPEN_BRACKETS:
                stack.append(node)
                child = Tree()
                child.tokens.append(Atom(*item))
                node.tokens.append(child)
                node = child
            else:
                child = Atom(*item)
                node.tokens.append(child)
                other = node.tokens[0]
                if other.tok != BRACKET:
                    raise ParserError(
                        child.lineno, child.column, child.start, child.end,
                        'unexpected %s' % val)
                if val in CLOSE_BRACKETS and val != CLOSE_BRACKET_MAP[other.val]:
                    raise UnbalancedParenthesisError(
                        child.lineno, child.column, child.start, child.end,
                        other.lineno, other.column, other.start, other.end,
                        CLOSE_BRACKET_MAP[other.val], val)
                node = stack.pop()
        else:
            node.tokens.append(Atom(*item))

    if stack:
        tree = find_last_tree(stack[-1])
        end = len(s)
        lineno = s.count('\n') + 1
        column = end - s.rfind('\n')
        other = tree.tokens[0]
        raise UnbalancedParenthesisError(
            lineno, column, end, end + 1,
            other.lineno, other.column, other.start, other.end,
            CLOSE_BRACKET_MAP[other.val], 'end of file')

    return node

def find_last_tree(node):
    while True:
        if not node.tokens:
            return node
        child = node.tokens[-1]
        if child.tok != TREE:
            return node
        node = child

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
                        raise ParserError(arg0.lineno, arg0.column, arg0.start, arg0.end, str(e))
                    else:
                        print("\tparsed format argument: %r" % arg0)
                for argind, arg in enumerate(args):
                    print("\targ %d: %s" % (argind, arg))
            print()
    except UnbalancedParenthesisError as e:
        illegal = Atom(e.start, e.end, e.lineno, e.column, ILLEGAL, s[e.start:e.end])
        print_errors(filename, s, [illegal], str(e))

        illegal = Atom(e.other_start, e.other_end, e.other_lineno, e.other_column, ILLEGAL, s[e.other_start:e.other_end])
        print_errors(filename, s, [illegal], "open bracket was here")

    except ParserError as e:
        illegal = Atom(e.start, e.end, e.lineno, e.column, ILLEGAL, s[e.start:e.end])
        print_errors(filename, s, [illegal], str(e))

RED = "\x1b[31m"
BLUE = "\x1b[34m"
NORMAL = "\x1b[0m"

class LineInfo:
    __slots__ = 'lineno', 'line', 'ranges'

    def __init__(self, lineno, line):
        self.lineno = lineno
        self.line = line
        self.ranges = []
    
    def add_range(self, start, end):
        i = 0
        while i < len(self.ranges):
            o_start, o_end = self.ranges[i]
            if start <= o_end and end >= o_end:
                if start < o_start:
                    self.ranges[i][0] = start
                self.ranges[i][1] = end
                i += 1
                while i < len(self.ranges):
                    next_start, next_end = self.ranges[i]
                    if next_start <= end:
                        del self.ranges[i]
                        if end < next_end:
                            end = next_end
                            self.ranges[i - 1][1] = end
                    else:
                        i += 1
                return
            elif start <= o_start and end >= o_start:
                self.ranges[i][0] = start
                if end > o_end:
                    self.ranges[i][1] = end
                return
            elif end < o_start:
                self.ranges.insert(i, [start, end])
                return
            i += 1
        self.ranges.append([start, end])

def gather_lines(s, toks):
    lines = s.split('\n')
    line_infos = {}
    for tok in toks:
        start_line = tok.lineno
        end_line = start_line + s.count('\n', tok.start, tok.end)
        for lineno in range(start_line, end_line + 1):
            if lineno in line_infos:
                info = line_infos[lineno]
                line = info.line
            else:
                line_index = lineno - 1
                line = lines[line_index]
                info = LineInfo(lineno, line)
                line_infos[lineno] = info
            
            start = 0 if lineno > start_line else tok.column - 1

            if lineno < end_line:
                end = len(line)
            else:
                line_start = s.rfind('\n', 0, tok.end) + 1
                end = tok.end - line_start
            
            info.add_range(start, end)
    return [line_infos[lineno] for lineno in sorted(line_infos)]

def print_errors(fielname, s, toks, message):
    if sys.stdout.isatty():
        red = RED
        blue = BLUE
        normal = NORMAL
    else:
        red = blue = normal = ""
    line_padd = 1 + len(str(toks[-1].lineno + s.count('\n', toks[-1].start, toks[-1].end)))
    print("%s:%d:%d" % (filename, toks[0].lineno, toks[0].column), message)
    infos = gather_lines(s, toks)
    for info in infos:
        line = info.line
        str_lineno = str(info.lineno)
        print('%s%s%s |%s %s' % (blue, ' ' * (line_padd - len(str_lineno)), str_lineno, normal, line.replace('\t', '    ')))
        buf = ['%s%s |%s ' % (blue, ' ' * line_padd, normal)]
        prev = 0
        for start, end in info.ranges:
            for i in range(prev, start):
                if i < len(line) and line[i] == '\t':
                    buf.append('    ')
                else:
                    buf.append(' ')
            buf.append(red)
            for i in range(start, max(end, start + 1)):
                if i < len(line) and line[i] == '\t':
                    buf.append('^^^^')
                else:
                    buf.append('^')
            buf.append(normal)
            prev = end
        print(''.join(buf))
    print()

# only to be used on a comma list in parenthesis
def find_before_comma(tokens):
    for i, tok in enumerate(tokens):
        if tok.tok == OPERATOR and tok.val == ',':
            return tokens[i - 1]
    if tokens[-1].tok == BRACKET and tokens[-1].val == ')':
        return tokens[-2]
    return tokens[-1]

def validate_gettext(s, filename, func_name, valid_keys):
    try:
        for ident_tok, args_tok in gettext(s, func_name):
            args = parse_comma_list(args_tok.tokens[1:-1])
            if not args:
                print_errors(filename, s, [ident_tok, args_tok], "no arguments")
            else:
                arg0 = args[0]
                if arg0.is_strings():
                    try:
                        str_arg0 = parse_strings(arg0.tokens)
                    except SyntaxError as e:
                        raise ParserError(arg0.lineno, arg0.column, arg0.start, arg0.end, str(e))
                    if str_arg0 not in valid_keys:
                        print_errors(filename, s, arg0.tokens, "not a know string reference")
                else:
                    print_errors(filename, s, arg0.tokens, "not a string literal")
    except UnbalancedParenthesisError as e:
        illegal = Atom(e.start, e.end, e.lineno, e.column, ILLEGAL, s[e.start:e.end])
        print_errors(filename, s, [illegal], str(e))

        illegal = Atom(e.other_start, e.other_end, e.other_lineno, e.other_column, ILLEGAL, s[e.other_start:e.other_end])
        print_errors(filename, s, [illegal], "open bracket was here")

    except ParserError as e:
        illegal = Atom(e.start, e.end, e.lineno, e.column, ILLEGAL, s[e.start:e.end])
        print_errors(filename, s, [illegal], str(e))

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
