#!/usr/bin/env python3

import re
import sys
import enum
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
    r'((?i:(?:[LuU]|u8)?"(?:[^"\n\\]|\\[\\?"'+"'"+r'rnabfvt]|\\[0-9]{1,3}|\\x[0-9a-fA-F]{2}|\\U[0-9a-fA-F]{8}|\\u[0-9a-fA-F]{4})*"|R"[^\n"]*"))|' + # string
    r"((?i:(?:[LuU]|u8)?'(?:[^'\n\\]|\\[\\?'"+'"'+r"rnabfvt]|\\[0-9]{1,3}|\\x[0-9a-fA-F]{2}|\\U[0-9a-fA-F]{8}|\\u[0-9a-fA-F]{4})*'))|" + # char (yes they can be more than on character in C)
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

class TOK(enum.IntEnum):
    STRING     =  1
    CHAR       =  2
    IDENT      =  3
    ML_COMMENT =  4
    COMMENT    =  5
    SPACE      =  6
    NUMBER     =  7
    TAG        =  8
    OPERATOR   =  9
    BRACKET    = 10
    PREPROC    = 11

    TREE    = -1
    ILLEGAL = -2

TOKENS = tuple(TOK.__members__.values())

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

def _parse_string_token_repl(m):
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

def parse_string_token(val):
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

    return prefix, STR_PART.sub(_parse_string_token_repl, s)

def _prefix_msg(prefix):
    return "string literal prefix " + prefix if prefix else "no string literal prefix"

def first_atom(node):
    if node.tok != TOK.TREE:
        return node
    return first_atom(node.tokens[0])

def parse_string(tokens):
    # NOTE: this accepts concatenation of incompatible string literlas, like: L"foo" "bar"
    buf = []
    prefix = None
    for tok in tokens:
        if tok.tok != TOK.STRING:
            raise UnexpectedTokenError(tok.lineno, tok.column, tok.end_lineno, tok.end_column,
                "string literal", first_atom(tok).val)
        try:
            p, val = parse_string_token(tok.val)
        except SyntaxError as e:
            raise ParserError(tok.lineno, tok.column, tok.end_lineno, tok.end_column, str(e))
        if prefix is None:
            prefix = p
        elif p is not None and p != prefix:
            raise UnexpectedTokenError(tok.lineno, tok.column, tok.end_lineno, tok.end_column,
                _prefix_msg(prefix), _prefix_msg(p))
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

        for tok in TOKENS:
            val = m.group(tok)
            if val is not None:
                yield Atom(lineno, column, end_lineno, end_column, TOK(tok), val)
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
        self.tok = TOK.TREE

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

IGNORE_TOKS = frozenset([TOK.ML_COMMENT, TOK.COMMENT, TOK.SPACE])

def parse(s):
    node = Tree()
    stack = []
    for atom in tokens(s):
        if atom.tok in IGNORE_TOKS:
            continue

        if atom.tok == TOK.BRACKET:
            val = atom.val
            if val in OPEN_BRACKETS:
                stack.append(node)
                child = Tree()
                child.tokens.append(atom)
                node.tokens.append(child)
                node = child
            else:
                child = atom
                if not node.tokens:
                    raise ParserError(
                        child.lineno, child.column, child.end_lineno, child.end_column,
                        'unexpected %s' % val)
                other = node.tokens[0]
                if other.tok != TOK.BRACKET:
                    raise ParserError(
                        child.lineno, child.column, child.end_lineno, child.end_column,
                        'unexpected %s' % val)
                if val in CLOSE_BRACKETS and val != CLOSE_BRACKET_MAP[other.val]:
                    raise UnbalancedParenthesisError(
                        child.lineno, child.column, child.end_lineno, child.end_column,
                        other.lineno, other.column, other.end_lineno, other.end_column,
                        CLOSE_BRACKET_MAP[other.val], val)
                node.tokens.append(child)
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
        if child.tok != TOK.TREE:
            return node
        node = child

def parse_comma_list(tokens):
    parsed = []
    if tokens:
        node = Tree()
        parsed.append(node)
        for tok in tokens:
            if tok.tok == TOK.OPERATOR and tok.val == ',':
                if not node.tokens:
                    raise UnexpectedTokenError(tok.lineno, tok.column,
                        tok.end_lineno, tok.end_column, "an expression", tok.val)
                node = Tree()
                parsed.append(node)
            else:
                node.tokens.append(tok)

        if not node.tokens:
            raise ParserError(tok.lineno, tok.column,
                tok.end_lineno, tok.end_column, "trailing comma")

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

def validate_gettext(s, filename, valid_keys, func_defs, only_errors=False, before=0, after=0, color=True):
    lines = split_lines(s)
    ok = True
    try:
        for ident_tok, args_tok, (min_argc, max_argc, key_index) in gettext(s, func_defs):
            args = parse_comma_list(args_tok.tokens[1:-1])
            argc = len(args)
            if argc < min_argc:
                print_mark(filename, lines, [ident_tok, *(args if args else args_tok.tokens)],
                    "not enough arguments (minimum are %d, but got %d)" % (min_argc, argc),
                    before=before, after=after, color=color)
                ok = False

            elif max_argc is not None and argc > max_argc:
                print_mark(filename, lines, [ident_tok, *args[max_argc:]],
                    "too many arguments (maximum are %d, but got %d)" % (max_argc, argc),
                    before=before, after=after, color=color)
                ok = False

            else:
                key_arg = args[key_index]
                if key_arg.tokens:
                    try:
                        key = parse_string(key_arg.tokens)
                    except ParserError as e:
                        illegal = Atom(e.lineno, e.column, e.end_lineno, e.end_column, TOK.ILLEGAL, e.str_slice(lines))
                        print_mark(filename, lines, [illegal], str(e), before=before, after=after, color=color)
                        ok = False

                    else:
                        if key not in valid_keys:
                            print_mark(filename, lines, key_arg.tokens, "not a know string key",
                                before=before, after=after, color=color)
                            ok = False

                        elif not only_errors:
                            print_mark(filename, lines, [ident_tok, *args_tok.tokens], "valid gettext invocation",
                                mark_color=GREEN, before=before, after=after, color=color)
                            print("\tparsed string key: %r" % key)
                            for argind, arg in enumerate(args):
                                print("\targument %d: %s" % (argind, arg))
                            print()
                else:
                    print_mark(filename, lines, key_arg.tokens, "expected a string literal",
                        before=before, after=after, color=color)
                    ok = False

    except UnbalancedParenthesisError as e:
        illegal = Atom(e.lineno, e.column, e.end_lineno, e.end_column, TOK.ILLEGAL, e.str_slice(lines))
        print_mark(filename, lines, [illegal], str(e), before=before, after=after, color=color)

        illegal = Atom(e.other_lineno, e.other_column, e.other_end_lineno, e.other_end_column,
            TOK.ILLEGAL, e.other_str_slice(lines))
        print_mark(filename, lines, [illegal], "open bracket was here",
            before=before, after=after, color=color)
        ok = False

    except ParserError as e:
        illegal = Atom(e.lineno, e.column, e.end_lineno, e.end_column, TOK.ILLEGAL, e.str_slice(lines))
        print_mark(filename, lines, [illegal], str(e), before=before, after=after, color=color)
        ok = False

    return ok

class LineInfo:
    __slots__ = 'lineno', 'line', 'ranges'

    def __init__(self, lineno, line):
        self.lineno = lineno
        self.line   = line
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
                        break
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

def gather_lines(lines, toks, before=0, after=0):
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
    
    for info in list(line_infos.values()):
        if before > 0:
            for lineno in range(max(info.lineno - before, 1), info.lineno):
                if lineno not in line_infos:
                    line_infos[lineno] = LineInfo(lineno, lines[lineno - 1])

        if after > 0:
            for lineno in range(info.lineno + 1, min(info.lineno + after, len(lines)) + 1):
                if lineno not in line_infos:
                    line_infos[lineno] = LineInfo(lineno, lines[lineno - 1])

    return [line_infos[lineno] for lineno in sorted(line_infos)]

def _flatten_tree(nodes, flat):
    for node in nodes:
        if node.tok == TOK.TREE:
            _flatten_tree(node.tokens, flat)
        else:
            flat.append(node)

def flatten_tree(nodes):
    flat = []
    _flatten_tree(nodes, flat)
    return flat

def print_mark(filename, lines, toks, message, mark_color=RED, lineno_color=BLUE, before=0, after=0, color=True):
    if color:
        normal = NORMAL
    else:
        mark_color = lineno_color = normal = ""
    toks = flatten_tree(toks)
    line_padd = 1 + len(str(toks[-1].end_lineno))
    print("%s:%d:%d: %s" % (filename, toks[0].lineno, toks[0].column, message))
    infos = gather_lines(lines, toks, before, after)
    for info in infos:
        line = info.line
        str_lineno = str(info.lineno)
        print('%s%s%s |%s %s' % (lineno_color, ' ' * (line_padd - len(str_lineno)), str_lineno, normal, line.replace('\t', '    ')))

        if info.ranges:
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

def gettext(s, func_defs='_'):
    node = parse(s)
    yield from _gettext(node, func_defs)

EXPR_KEYWORDS = frozenset(['return', 'case', 'goto', 'sizeof', '__typeof__', '__typeof', 'typeof'])

def _gettext(node, func_defs):
    i = 0
    while i < len(node.tokens):
        child = node.tokens[i]
        if child.tok == TOK.IDENT:
            # NOTE: This can distinguish between _() and foo->_() / foo._() / foo::_(),
            #       but it cannot detect if _ is a local variable, since that would
            #       need a full blown Objective-C++ parser.
            if i > 0:
                prev = node.tokens[i - 1]
                if prev.tok == TOK.OPERATOR:
                    if prev.val in ('->', '.'):
                        # method invocation
                        i += 1
                        continue

                    if i - 1 > 0 and prev.val == '::':
                        # C++ namespaced function invocation
                        prev = node.tokens[i - 2]
                        if prev.tok == TOK.IDENT:
                            i += 1
                            continue

                elif prev.tok == TOK.IDENT and prev.val not in EXPR_KEYWORDS:
                    # function declaration/definition
                    i += 1
                    continue

            func_def = func_defs.get(child.val)
            if func_def is not None and i + 1 < len(node.tokens) and node.tokens[i + 1].tok == TOK.TREE:
                next_child = node.tokens[i + 1]
                if next_child.tokens[0].val == '(':
                    yield child, next_child, func_def
                    i += 1

        elif child.tok == TOK.TREE:
            yield from _gettext(child, func_defs)
        i += 1

def parse_func_defs(s):
    func_defs = {}

    for str_def in s.split(','):
        name, *args = str_def.split('/')
        if name in func_defs:
            raise KeyError('doubled function name: ' + name)

        if not name:
            raise ValueError('function name may not be empty')

        min_argc  = 1
        max_argc  = 1
        key_index = 0

        if args:
            argc = args[0].split('-')
            if len(argc) > 1:
                min_argc = int(argc[0])
                max_argc = int(argc[1]) if argc[1] != '*' else None
            elif argc[0] == '*':
                min_argc = 1
                max_argc = None
            else:
                min_argc = max_argc = int(argc[0])

            if min_argc < 1:
                raise ValueError(
                    'function %s: minimum argument count (%d) has to be bigger than 1' %
                     (name, min_argc))

            if max_argc is not None and max_argc < min_argc:
                raise ValueError(
                    'function %s: minimum argument count (%d) has to be smaller than maximum argument count (%d)' %
                     (name, min_argc, max_argc))

            if len(args) > 1:
                key_index = int(args[1])

            if max_argc is not None and key_index >= max_argc:
                raise ValueError(
                    'function %s: string key index (%d) has to be smaller than maximum argument count (%d)' %
                     (name, key_index, max_argc))

        func_defs[name] = (min_argc, max_argc, key_index)

    return func_defs

def main(args):
    import argparse

    parser = argparse.ArgumentParser(
        prog='validate-gettext.py',
        formatter_class=argparse.RawTextHelpFormatter)

    parser.add_argument('valid_keys',
        help='file with known gettext string keys, one key per line')

    parser.add_argument('source', nargs='+',
        help='C/C++/Objective-C source file')

    parser.add_argument('--func-defs', default='_/1/0,gettext/1/0',
        help='comma separated list of function definitions:\n'
            '<FUNC_NAME>[/<MIN_ARGC>-<MAX_ARGC>[/<KEY_INDEX>]] or\n'
            '<FUNC_NAME>[/<MIN_ARGC>-*[/<KEY_INDEX>]] or\n'
            '<FUNC_NAME>[/<ARGC>[/<KEY_INDEX>]]\n'
            '\n'
            'Example:\n'
            'validate-gettext.py --func-defs _/1/0,gettext/1/0 ...\n\n')

    parser.add_argument('--only-errors', default=False, action='store_true',
        help='show only errors, no valid gettext invokations')

    parser.add_argument('--before', type=int, default=0, metavar='LINES',
        help='lines of context before a marked source location to show')

    parser.add_argument('--after', type=int, default=0, metavar='LINES',
        help='lines of context after a marked source location to show')

    parser.add_argument('--color', choices=['always', 'never', 'auto'], default='auto',
        help='wether to colorize the output')

    opts = parser.parse_args(args)

    with open(opts.valid_keys) as fp:
        valid_keys = set(line.rstrip('\n') for line in fp)

    if '' in valid_keys:
        valid_keys.remove('')

    if opts.color == 'auto':
        color = sys.stdout.isatty()
    else:
        color = opts.color == 'always'

    try:
        func_defs = parse_func_defs(opts.func_defs)
    except (ValueError, KeyError) as e:
        print(e)
        return 2

    status = 0
    for source in opts.source:
        with open(source) as fp:
            s = fp.read()
        if not validate_gettext(s, source, valid_keys,
                func_defs = func_defs,
                only_errors = opts.only_errors,
                before = opts.before,
                after = opts.after,
                color = color):
            status = 1
    return status

if __name__ == '__main__':
    sys.exit(main(sys.argv[1:]))