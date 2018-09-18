#!/usr/bin/env python3

import re
import sys
import string

RED     = "\x1b[31m"
GREEN   = "\x1b[32m"
YELLOW  = "\x1b[33m"
BLUE    = "\x1b[34m"
MAGENTA = "\x1b[35m"
CYAN    = "\x1b[36m"
NORMAL  = "\x1b[0m"

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
        prefix = None
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
        elif p is not None and p != prefix:
            raise UnexpectedTokenError(tok.lineno, tok.column, tok.end_lineno, tok.end_column, _prefix_msg(prefix), _prefix_msg(p))
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
            raise UnexpectedTokenError(lineno, column, lineno, column + 1, 'a valid token', s[index:index + 1])

        val = m.group(0)
        newlines = val.count("\n")
        if newlines > 0:
            end_lineno = lineno + newlines
            end_column = len(val) - val.rfind("\n")
        else:
            end_lineno = lineno
            end_column = column + len(val)

        for TOK in TOKENS:
            val = m.group(TOK)
            if val is not None:
                yield Atom(lineno, column, end_lineno, end_column, TOK, val)
                break

        lineno = end_lineno
        column = end_column

        index = m.end()

class Node:
    __slots__ = ()

    @property
    def tok(self):
        raise NotImplementedError

    @property
    def lineno(self):
        raise NotImplementedError

    @property
    def column(self):
        raise NotImplementedError

    @property
    def end_lineno(self):
        raise NotImplementedError

    @property
    def end_column(self):
        raise NotImplementedError

    def slice(self, lines):
        return slice_lines(lines, self.lineno, self.column, self.end_lineno, self.end_column)

    def str_slice(self, lines):
        return ''.join(self.slice(lines))

class Atom(Node):
    __slots__ = 'lineno', 'column', 'end_lineno', 'end_column', 'tok', 'val'

    def __init__(self, lineno, column, end_lineno, end_column, tok, val):
        self.lineno = lineno
        self.column = column
        self.end_lineno = end_lineno
        self.end_column = end_column
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
    def lineno(self):
        return self.tokens[0].lineno

    @property
    def column(self):
        return self.tokens[0].column

    @property
    def end_lineno(self):
        return self.tokens[-1].end_lineno

    @property
    def end_column(self):
        return self.tokens[-1].end_column

    def is_strings(self):
        for tok in self.tokens:
            if tok.tok != STRING:
                return False
        return True

class ParserError(Exception):
    def __init__(self, lineno, column, end_lineno, end_column, message):
        Exception.__init__(self, message)
        self.lineno = lineno
        self.column = column
        self.end_lineno = end_lineno
        self.end_column = end_column

    def slice(self, lines):
        return slice_lines(lines, self.lineno, self.column, self.end_lineno, self.end_column)

    def str_slice(self, lines):
        return ''.join(self.slice(lines))

class UnexpectedTokenError(ParserError):
    def __init__(self, lineno, column, end_lineno, end_column, expected, got):
        ParserError.__init__(self, lineno, column, end_lineno, end_column,
            "expected %s, but got %s" % (expected, got))
        self.expected = expected
        self.got = got

class UnbalancedParenthesisError(ParserError):
    def __init__(self, lineno, column, end_lineno, end_column,
            other_lineno, other_column, other_end_lineno, other_end_column,
            expected, got):
        ParserError.__init__(self, lineno, column, end_lineno, end_column,
            "expected %s, but got %s" % (expected, got))
        self.other_lineno = other_lineno
        self.other_column = other_column
        self.other_end_lineno = other_end_lineno
        self.other_end_column = other_end_column

    def other_slice(self, lines):
        return slice_lines(lines, self.other_lineno, self.other_column, self.other_end_lineno, self.other_end_column)

    def other_str_slice(self, lines):
        return ''.join(self.other_slice(lines))

def parse(s):
    node = Tree()
    stack = []
    for atom in tokens(s):
        if atom.tok in (ML_COMMENT, COMMENT, SPACE):
            continue

        if atom.tok == BRACKET:
            val = atom.val
            if val in OPEN_BRACKETS:
                stack.append(node)
                child = Tree()
                child.tokens.append(atom)
                node.tokens.append(child)
                node = child
            else:
                child = atom
                node.tokens.append(child)
                other = node.tokens[0]
                if other.tok != BRACKET:
                    raise ParserError(
                        child.lineno, child.column, child.end_lineno, child.end_column,
                        'unexpected %s' % val)
                if val in CLOSE_BRACKETS and val != CLOSE_BRACKET_MAP[other.val]:
                    raise UnbalancedParenthesisError(
                        child.lineno, child.column, child.end_lineno, child.end_column,
                        other.lineno, other.column, other.end_lineno, other.end_column,
                        CLOSE_BRACKET_MAP[other.val], val)
                node = stack.pop()
        else:
            node.tokens.append(atom)

    if stack:
        lines = split_lines(s)
        tree = find_last_tree(stack[-1])
        lineno = len(lines) + 1
        column = len(lines[-1])
        other = tree.tokens[0]
        raise UnbalancedParenthesisError(
            lineno, column, lineno, column + 1,
            other.lineno, other.column, other.end_lineno, other.end_column,
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

NON_JOIN_CHARS = WORD_CHARS | {'"', "'"}

def join_tokens(tokens):
    buf = [str(tok) for tok in tokens]
    i = 1
    while i < len(buf):
        c1 = buf[i - 1][-1]
        c2 = buf[i][0]
        if c1 in NON_JOIN_CHARS and c2 in NON_JOIN_CHARS:
            buf.insert(i, ' ')
            i += 1
        i += 1
    return ''.join(buf)

def slice_lines(lines, start_lineno, start_column, end_lineno, end_column):
    buf = []
    for i in range(start_lineno, end_lineno + 1):
        if i == end_lineno:
            buf.append(lines[i][start_column - 1 : end_column - 1])
        else:
            buf.append(lines[i][start_column - 1:])
        start_column = 0
    return buf

def split_lines(s):
    if s.endswith("\n"):
        s = s[:-1]
    lines = s.split("\n")
    if not lines:
        lines.append("")
    return lines

def validate_gettext(s, filename, valid_keys, func_name='_', only_errors=False):
    lines = split_lines(s)
    ok = True
    try:
        for ident_tok, args_tok in gettext(s, func_name):
            args = parse_comma_list(args_tok.tokens[1:-1])
            if not args:
                print_mark(filename, lines, [ident_tok, args_tok], "no arguments")
            else:
                arg0 = args[0]
                if arg0.is_strings():
                    try:
                        arg0_str = parse_strings(arg0.tokens)
                    except SyntaxError as e:
                        print_mark(filename, lines, arg0.tokens, str(e))
                        ok = False
                    except ParserError as e:
                        illegal = Atom(e.lineno, e.column, e.end_lineno, e.end_column, ILLEGAL, e.str_slice(lines))
                        print_mark(filename, lines, [illegal], str(e))
                        ok = False
                    else:
                        if arg0_str not in valid_keys:
                            print_mark(filename, lines, arg0.tokens, "not a know string key")
                            ok = False
                        elif not only_errors:
                            print_mark(filename, lines, [ident_tok, *args_tok.tokens], "valid gettext invocation", GREEN)
                            print("\tparsed format argument: %r" % arg0_str)
                            for argind, arg in enumerate(args):
                                print("\targument %d: %s" % (argind, arg))
                            print()
                else:
                    print_mark(filename, lines, arg0.tokens, "not a string literal")
                    ok = False
    except UnbalancedParenthesisError as e:
        illegal = Atom(e.lineno, e.column, e.end_lineno, e.end_column, ILLEGAL, e.str_slice(lines))
        print_mark(filename, lines, [illegal], str(e))

        illegal = Atom(e.other_lineno, e.other_column, e.other_end_lineno, e.other_end_column, ILLEGAL, e.other_str_slice(lines))
        print_mark(filename, lines, [illegal], "open bracket was here")
        ok = False
    return ok

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

def gather_lines(lines, toks):
    line_infos = {}
    for tok in toks:
        start_lineno = tok.lineno
        end_lineno = tok.end_lineno
        for lineno in range(start_lineno, end_lineno + 1):
            if lineno in line_infos:
                info = line_infos[lineno]
                line = info.line
            else:
                line_index = lineno - 1
                line = lines[line_index]
                info = LineInfo(lineno, line)
                line_infos[lineno] = info

            start = 0 if lineno > start_lineno else tok.column - 1

            if lineno < end_lineno:
                end = len(line)
            else:
                end = tok.end_column - 1

            info.add_range(start, end)
    return [line_infos[lineno] for lineno in sorted(line_infos)]

def print_mark(filename, lines, toks, message, mark_color=RED, lineno_color=BLUE):
    if sys.stdout.isatty():
        normal = NORMAL
    else:
        mark_color = lineno_color = normal = ""
    line_padd = 1 + len(str(toks[-1].end_lineno))
    print("%s:%d:%d" % (filename, toks[0].lineno, toks[0].column), message)
    infos = gather_lines(lines, toks)
    for info in infos:
        line = info.line
        str_lineno = str(info.lineno)
        print('%s%s%s |%s %s' % (lineno_color, ' ' * (line_padd - len(str_lineno)), str_lineno, normal, line.replace('\t', '    ')))
        buf = ['%s%s |%s ' % (lineno_color, ' ' * line_padd, normal)]
        prev = 0
        for start, end in info.ranges:
            for i in range(prev, start):
                if i < len(line) and line[i] == '\t':
                    buf.append('    ')
                else:
                    buf.append(' ')
            buf.append(mark_color)
            for i in range(start, max(end, start + 1)):
                if i < len(line) and line[i] == '\t':
                    buf.append('^^^^')
                else:
                    buf.append('^')
            buf.append(normal)
            prev = end
        print(''.join(buf))
    print()

def gettext(s, func_name='_'):
    node = parse(s)
    yield from _gettext(node, func_name)

EXPR_KEYWORS = frozenset(['return', 'case', 'goto', 'sizeof', '__typeof__', '__typeof', 'typeof'])

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
                        # method invocation
                        i += 1
                        continue

                    if i - 1 > 0 and prev.val == '::':
                        # C++ namespaced function invocation
                        prev = node.tokens[i - 2]
                        if prev.tok == IDENT:
                            i += 1
                            continue

                elif prev.tok == IDENT and prev.val not in EXPR_KEYWORS:
                    # function declaration/definition
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

def main(args):
    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument('valid_keys')
    parser.add_argument('source', nargs='+')
    parser.add_argument('--func-name', default='_')
    parser.add_argument('--only-errors', default=False, action='store_true')
    opts = parser.parse_args(args)

    func_name = opts.func_name
    with open(opts.valid_keys) as fp:
        valid_keys = set(line.rstrip('\n') for line in fp)

    if '' in valid_keys:
        valid_keys.remove('')

    status = 0
    for source in opts.source:
        with open(source) as fp:
            s = fp.read()
        if not validate_gettext(s, source, valid_keys, func_name, opts.only_errors):
            status = 1
    return status

if __name__ == '__main__':
    sys.exit(main(sys.argv[1:]))