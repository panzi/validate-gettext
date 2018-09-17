#!/usr/bin/env python3

import re
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
            raise ValueError("not a string token: %s" + tok)
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

def skip(it):
    while True:
        item = next(it)
        if item[4] not in (ML_COMMENT, COMMENT, SPACE):
            return item

class Node:
    __slots__ = ()

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

    def is_strings(self):
        for tok in self.tokens:
            if tok.tok != STRING:
                return False
        return True

class ParserError(Exception):
    def __init__(self, lineno, column, start, end, expected, got):
        Exception.__init__(self, "line %d column %d: expected %s, but got %s" % (
            lineno, column, expected, got))
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

def gettext(s, func_name='_'):
    node = parse(s)
    _gettext(node, func_name)

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
                    print('gettext at line %d column %d: _%s' % (child.lineno, child.column,
                        join_tokens(next_child.tokens)))

                    args = parse_comma_list(next_child.tokens[1:-1])
                    if not args:
                        print("\tNO ARGUMENTS!")
                    else:
                        arg0 = args[0]
                        if arg0.is_strings():
                            arg0 = parse_strings(arg0.tokens)
                            print("\tparsed format argument: %r" % arg0)
                        for argind, arg in enumerate(args):
                            print("\targ %d: %s" % (argind, arg))
                    print()
                    i += 1
                
        elif child.tok == TREE:
            _gettext(child)
        i += 1

if __name__ == '__main__':
    import sys
    with open(sys.argv[1]) as fp:
        s = fp.read()
    gettext(s)
