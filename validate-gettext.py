#!/usr/bin/env python3

import re
import sys
import enum
import string
import collections

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
# FIXME: Raw strings work differently. R"([^\s(){}\[\]]*)\(([\n.]*?)\)\1"
LEX = re.compile(
    r'((?i:(?:[LuU]|u8)?"(?:[^"\n\\]|\\[\\?"'+"'"+r'rnabfvt]|\\[0-9]{1,3}|\\x[0-9a-fA-F]{2}|\\U[0-9a-fA-F]{8}|\\u[0-9a-fA-F]{4})*"))|' + # string
    r'((?i:(?:[LuU]|u8)?R"([^\s(){}\[\]]*)\((?:\n|.)*?\)\3"))|' + # raw string
    r"((?i:(?:[LuU]|u8)?'(?:[^'\n\\]|\\[\\?'"+'"'+r"rnabfvt]|\\[0-9]{1,3}|\\x[0-9a-fA-F]{2}|\\U[0-9a-fA-F]{8}|\\u[0-9a-fA-F]{4})*'))|" + # char (yes they can be more than on character in C)
    r'([_a-z][_a-z0-9]*)|' +        # identifier
    r'(/\*(?:[^*]|\*(?!/))*\*/)|' + # multiline comment
    r'(//[^\n]*)|' +                # single line comment
    r'([ \v\t\r\n]+)|' +            # space
    r'([-+]?[0-9]+(?:\.[0-9]*)?(?:e[-+]?[0-9]+)?i?|[-+]?0x[0-9a-f]+|[-+]?[0-9]*\.[0-9]+(?:e[-+]?[0-9]+)?i?)|' + # numbers
    r'(@[_a-z][_a-z0-9]*)|' + # tag
    r'([-+*/%&|^!=<>]=|<<=?|>>=?|\|\||&&|->|\.\.\.|--|\+\+|##|::|[-+./*,!^~<>|&?:%=;@])|' + # operators
    r'([{}()\[\]])|' + # brackets
    r'((?i:#[ \t]*\d+[ \t]*"(?:[^"\n\\]|\\[\\?"'+"'"+r'rnabfvt]|\\[0-9]{1,3}|\\x[0-9a-fA-F]{2})*"[^\n]*$))|' + # source map
    r'(#(?:\\\n|[^\n])*)|' + # preproc
    r'(\\\n)', # line continuation
    re.M | re.I)

class TOK(enum.IntEnum):
    STRING     =  1
    RAW_STRING =  2 # 3 is part of raw string
    CHAR       =  4
    IDENT      =  5
    ML_COMMENT =  6
    COMMENT    =  7
    SPACE      =  8
    NUMBER     =  9
    TAG        = 10
    OPERATOR   = 11
    BRACKET    = 12
    SOURCE_MAP = 13
    PREPROC    = 14
    LINE_CONT  = 15

    TREE    = -1
    ILLEGAL = -2

TOKENS = tuple(tok for tok in TOK.__members__.values() if tok > 0)

IGNORE_TOKS = frozenset([TOK.ML_COMMENT, TOK.COMMENT, TOK.SPACE, TOK.LINE_CONT])

STR_PART = re.compile(r'([^"\n\\])|\\([?"'+"'"+r'rnabfvt])|\\([0-9]{1,3})|\\x([0-9a-fA-F]{2})|\\U([0-9a-fA-F]{8})|\\u([0-9a-fA-F]{4})')
RAW_STR  = re.compile(r'^([LuU]|u8)?R"([^\s(){}\[\]]*)\(((?:\n|.)*?)\)\2"$', re.M)
NON_SPACE = re.compile('[^ ]')

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

def parse_raw_string_token(val):
    m = RAW_STR.match(val)
    return m.group(1), m.group(3)

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
    elif val.startswith('"'):
        prefix = None
        s = val[1:-1]
    else:
        raise SyntaxError('illegal string literal: ' + val)

    return prefix, STR_PART.sub(_parse_string_token_repl, s)

def _prefix_msg(prefix):
    return "string literal prefix " + prefix if prefix else "no string literal prefix"

def find_first_atom(node):
    if node.tok != TOK.TREE:
        return node
    return find_first_atom(node.tokens[0])

def parse_string(tokens):
    buf = []
    prefix = None
    for tok in tokens:
        try:
            if tok.tok == TOK.RAW_STRING:
                p, val = parse_raw_string_token(tok.val)
            elif tok.tok == TOK.STRING:
                p, val = parse_string_token(tok.val)
            else:
                raise UnexpectedTokenError(tok, "string literal", find_first_atom(tok).val)
        except SyntaxError as e:
            raise ParserError(tok, str(e))

        if prefix is None:
            prefix = p
        elif p is not None and p != prefix:
            raise UnexpectedTokenError(tok, _prefix_msg(prefix), _prefix_msg(p))
        buf.append(val)

    return ''.join(buf)

SOURCE_MAP = re.compile(r'^#[ \t]*(\d+)[ \t]*("(?:[^"\n\\]|\\[\\?"'+"'"+
    r'rnabfvt]|\\[0-9]{1,3}|\\x[0-9a-fA-F]{2})*")((?:[ \t]*\d+)*)')

def parse_source_map_line(src):
    # see: https://gcc.gnu.org/onlinedocs/cpp/Preprocessor-Output.html
    m = SOURCE_MAP.match(src)
    _, filename = parse_string_token(m.group(2))
    flags = set(map(int, m.group(3).split()))
    return filename, int(m.group(1)), flags

def tokenize(s, filename):
    index = 0
    n = len(s)
    lineno = 1
    column = 1
    map_filename = filename
    map_src_lineno = 1
    src_lineno = 1
    while index < n:
        m = LEX.match(s, index)

        if not m:
            err_lineno = lineno - src_lineno + map_src_lineno
            err_column = column

            illegal = Atom(map_filename, err_lineno, err_column, err_lineno, err_column + 1,
                TOK.ILLEGAL, s[index:index + 1])

            raise UnexpectedTokenError(illegal, 'a valid token', s[index:index + 1])

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
                diff = map_src_lineno - src_lineno
                atom = Atom(map_filename, lineno + diff, column, end_lineno + diff, end_column, TOK(tok), val)

                if atom.tok == TOK.SOURCE_MAP:
                    src_lineno = lineno + 1
                    map_filename, map_src_lineno, _ = parse_source_map_line(val)
                else:
                    yield atom
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

    def start_pos(self):
        return (self.lineno, self.column)

    def end_pos(self):
        return (self.end_lineno, self.end_column)

class Atom(Node):
    __slots__ = 'filename', 'lineno', 'column', 'end_lineno', 'end_column', 'tok', 'val'

    def __init__(self, filename, lineno, column, end_lineno, end_column, tok, val):
        self.filename = filename
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
    def __init__(self, token, message):
        Exception.__init__(self, message)
        self.token = token

class UnexpectedTokenError(ParserError):
    def __init__(self, token, expected, got):
        ParserError.__init__(self, token, "expected %s, but got %s" % (expected, got))
        self.expected = expected
        self.got = got

class UnbalancedParenthesisError(ParserError):
    def __init__(self, token, other_token, expected, got):
        ParserError.__init__(self, token, "expected %s, but got %s" % (expected, got))
        self.other_token = other_token

def parse(s, filename):
    node = Tree()
    stack = []
    atom = None
    for atom in tokenize(s, filename):
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
                    raise ParserError(child, 'unexpected %s' % val)
                other = node.tokens[0]
                if other.tok != TOK.BRACKET:
                    raise ParserError(child, 'unexpected %s' % val)
                if val in CLOSE_BRACKETS and val != CLOSE_BRACKET_MAP[other.val]:
                    raise UnbalancedParenthesisError(child, other,
                        CLOSE_BRACKET_MAP[other.val], val)
                node.tokens.append(child)
                node = stack.pop()
        else:
            node.tokens.append(atom)

    if stack:
        tree = find_last_tree(stack[-1])
        other = tree.tokens[0]
        eof = Atom(atom.filename, atom.end_lineno, atom.end_column, atom.end_lineno, atom.end_column + 1, TOK.ILLEGAL, '')
        raise UnbalancedParenthesisError(eof, other,
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
                    raise UnexpectedTokenError(tok, "an expression", tok.val)
                node = Tree()
                parsed.append(node)
            else:
                node.tokens.append(tok)

        if not node.tokens:
            raise ParserError(tok, "trailing comma")

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

def split_lines(s):
    if s.endswith("\n"):
        s = s[:-1]
    lines = s.split("\n")
    if not lines:
        lines.append("")
    return lines

def parse_source_map(filename, lines):
    map_files = {filename: lines}
    map_src_lineno = 0
    src_lineno = 0
    map_lines = None
    for i, line in enumerate(lines):
        lineno = i + 1
        m = SOURCE_MAP.match(line)
        if m:
            _, map_filename = parse_string_token(m.group(2))
            src_lineno = lineno + 1
            map_src_lineno = int(m.group(1))
            if map_filename in map_files:
                map_lines = map_files[map_filename]
            else:
                map_lines = map_files[map_filename] = []
        elif map_lines is not None:
            map_lineno = lineno - src_lineno + map_src_lineno
            map_line_index = map_lineno - 1
            while map_line_index >= len(map_lines):
                map_lines.append('')
            map_line = map_lines[map_line_index] # pylint: disable=E1136

            if map_line:
                m = NON_SPACE.search(line)
                if m:
                    non_space_index = m.start()
                    map_line_len = len(map_line)
                    diff = map_line_len - non_space_index
                    if diff >= 0:
                        # Generated lines that represent one single source line overlap.
                        # This manipulates the input lines so they don't overlap and can
                        # be merged into one for printing.
                        lines[i] = line = (' ' * (diff + 1)) + line
                    map_lines[map_line_index] = map_line + line[map_line_len:] # pylint: disable=E1137
                else:
                    map_lines[map_line_index] = map_line # pylint: disable=E1137
            else:
                map_lines[map_line_index] = line # pylint: disable=E1137

    return map_files

def validate_gettext(s, filename, valid_keys, func_defs, only_errors=False, before=0, after=0, color=True):
    key_count = {key: 0 for key in valid_keys}
    lines = split_lines(s)
    map_files = parse_source_map(filename, lines)
    src_files = {filename: lines}
    ok = True
    opts = dict(
        map_files=map_files, src_files=src_files,
        before=before, after=after, color=color
    )
    # lines might be manipulated by parse_source_map()
    s = '\n'.join(lines)
    try:
        for ident_tok, args_tok, (min_argc, max_argc, key_index) in find_gettext(s, filename, func_defs):
            args = parse_comma_list(args_tok.tokens[1:-1])
            argc = len(args)
            if argc < min_argc:
                print_mark([ident_tok, *(args if args else args_tok.tokens)],
                    "not enough arguments (minimum are %d, but got %d)" % (min_argc, argc),
                    **opts)
                ok = False

            elif max_argc is not None and argc > max_argc:
                print_mark([ident_tok, *args[max_argc:]],
                    "too many arguments (maximum are %d, but got %d)" % (max_argc, argc),
                    **opts)
                ok = False

            else:
                key_arg = args[key_index]
                if key_arg.tokens:
                    try:
                        key = parse_string(key_arg.tokens)
                    except ParserError as e:
                        print_mark([e.token], str(e), **opts)
                        ok = False

                    else:
                        if key not in valid_keys:
                            print_mark(key_arg.tokens, "not a know string key",
                                **opts)
                            ok = False

                        else:
                            key_count[key] += 1
                            if not only_errors:
                                print_mark([ident_tok, *args_tok.tokens], "valid gettext invocation",
                                    mark_color=GREEN, **opts)
                                print("\tparsed string key: %r" % key)
                                for argind, arg in enumerate(args):
                                    print("\targument %d: %s" % (argind, arg))
                                print()
                else:
                    print_mark(key_arg.tokens, "expected a string literal", **opts)
                    ok = False

    except UnbalancedParenthesisError as e:
        print_mark([e.token], str(e), **opts)
        print_mark([e.other_token], "open bracket was here", **opts)
        ok = False

    except ParserError as e:
        print_mark([e.token], str(e), **opts)
        ok = False

    return ok, key_count

def stringify(s):
    buf = ['"']
    for c in s.encode():
        if c == 0:
            buf.append('\\0')
        elif c == 0x07:
            buf.append('\\a')
        elif c == 0x08:
            buf.append('\\b')
        if c == 0x09:
            buf.append('\\t')
        if c == 0x0A:
            buf.append('\\n')
        if c == 0x0B:
            buf.append('\\v')
        if c == 0x0C:
            buf.append('\\f')
        if c == 0x0D:
            buf.append('\\r')
        if c == 0x5C:
            buf.append('\\\\')
        elif c >= 0x20 and c <= 0x7e:
            buf.append(chr(c))
        else:
            buf.append('\\x%02x' % c)
    buf.append('"')
    return ''.join(buf)

class LineInfo:
    __slots__ = 'lineno', 'line', 'filename', 'ranges'

    def __init__(self, lineno, line, filename):
        self.lineno   = lineno
        self.line     = line
        self.ranges   = []
        self.filename = filename
    
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

def gather_lines(toks, map_files, src_files, before=0, after=0):
    def get_src_lines(src_filename):
        if src_filename in src_files:
            src_lines = src_files[src_filename]
        else:
            with open(src_filename) as fp:
                src_lines = split_lines(fp.read())

            src_files[src_filename] = src_lines
        return src_lines

    line_infos = {}
    for tok in toks:
        map_filename = tok.filename
        start_lineno, start_column = tok.start_pos()
        end_lineno, end_column = tok.end_pos()

        src_lines = map_files[map_filename]
        line_count = len(src_lines)
        for lineno in range(start_lineno, end_lineno + 1):
            line_index = lineno - 1
            line = src_lines[line_index] if line_index != line_count else ''

            if lineno in line_infos:
                info = line_infos[lineno]
            else:
                info = LineInfo(lineno, line, map_filename)
                line_infos[lineno] = info

            start = 0 if lineno > start_lineno else start_column - 1

            if lineno < end_lineno:
                end = len(line)
            else:
                end = end_column - 1

            info.add_range(start, end)

    for info in list(line_infos.values()):
        src_lines = get_src_lines(info.filename)

        if before > 0:
            for lineno in range(max(info.lineno - before, 1), info.lineno):
                if lineno not in line_infos:
                    line_infos[lineno] = LineInfo(lineno, src_lines[lineno - 1], info.filename)

        if after > 0:
            for lineno in range(info.lineno + 1, min(info.lineno + after, len(src_lines)) + 1):
                if lineno not in line_infos:
                    line_infos[lineno] = LineInfo(lineno, src_lines[lineno - 1], info.filename)

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

def print_mark(toks, message, map_files, src_files, mark_color=RED, lineno_color=BLUE, before=0, after=0, color=True):
    if color:
        normal = NORMAL
    else:
        mark_color = lineno_color = normal = ""
    toks = flatten_tree(toks)
    last_lineno, _ = toks[-1].end_pos()
    line_padd = 1 + len(str(last_lineno))

    first_tok = toks[0]
    first_lineno, first_column = first_tok.start_pos()

    map_filename = first_tok.filename

    print("%s:%d:%d: %s" % (map_filename, first_lineno, first_column, message))
    infos = gather_lines(toks, map_files, src_files, before, after)
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

def find_gettext(s, filename, func_defs):
    node = parse(s, filename)
    yield from _find_gettext(node, func_defs)

EXPR_KEYWORDS = frozenset(['return', 'case', 'goto', 'sizeof', '__typeof__', '__typeof', 'typeof'])
METHOD_SPECIFIERS = frozenset(['throw', 'noexcept', 'const'])

def _find_gettext(node, func_defs):
    i = 0
    while i < len(node.tokens):
        child = node.tokens[i]
        if child.tok == TOK.IDENT and i + 1 < len(node.tokens):
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
                    # function declaration/definition `foo _()`
                    i += 1
                    continue

            args_tok = node.tokens[i + 1]

            if args_tok.tok != TOK.TREE:
                # no parenthesis `( )`
                i += 1
                continue

            paren_tok = args_tok.tokens[0]
            if paren_tok.tok != TOK.BRACKET or paren_tok.val != '(':
                # still no parenthesis `( )`
                i += 1
                continue

            if i + 2 < len(node.tokens):
                next_atom = find_first_atom(node.tokens[i + 2])

                if next_atom.tok == TOK.BRACKET and next_atom.val == '{':
                    # function declaration/definition `_() {`
                    i += 1
                    continue

                elif next_atom.tok == TOK.IDENT and next_atom.val in METHOD_SPECIFIERS:
                    # method declaration/definition `_() const` / `_() throw` / `_() noexcept`
                    i += 1
                    continue

            func_def = func_defs.get(child.val)
            if func_def is not None:
                yield child, args_tok, func_def
                i += 1

        elif child.tok == TOK.TREE:
            yield from _find_gettext(child, func_defs)
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
    import argparse, subprocess

    parser = argparse.ArgumentParser(
        prog='validate-gettext.py',
        formatter_class=argparse.RawTextHelpFormatter)

    parser.add_argument('valid_keys',
        help='file with known gettext string keys, one key per line')

    parser.add_argument('source', nargs='+',
        help='C/C++/Objective-C source file')

    parser.add_argument('-f', '--func-defs', default='_/1/0,gettext/1/0',
        help='comma separated list of function definitions:\n'
            '<FUNC_NAME>[/<MIN_ARGC>-<MAX_ARGC>[/<KEY_INDEX>]] or\n'
            '<FUNC_NAME>[/<MIN_ARGC>-*[/<KEY_INDEX>]] or\n'
            '<FUNC_NAME>[/<ARGC>[/<KEY_INDEX>]]\n'
            '\n'
            'Example:\n'
            'validate-gettext.py --func-defs _/1/0,gettext/1/0 ...\n\n')

    parser.add_argument('-e', '--only-errors', default=False, action='store_true',
        help='show only errors, no valid gettext invokations')

    parser.add_argument('-b', '--before', type=int, default=0, metavar='LINES',
        help='lines of context before a marked source location to show')

    parser.add_argument('-a', '--after', type=int, default=0, metavar='LINES',
        help='lines of context after a marked source location to show')

    parser.add_argument('-c', '--color', choices=['always', 'never', 'auto'], default='auto',
        help='wether to colorize the output')

    parser.add_argument('-P', '--preproc', help='pass source through a preprocessor')
    parser.add_argument('-A', '--preproc-arg', action="append")

    opts = parser.parse_args(args)

    with open(opts.valid_keys) as fp:
        data = fp.read()

    key_filename = opts.valid_keys.lower()
    if key_filename.endswith('.po') or key_filename.endswith('.pot'):
        strs = parse_po(data, opts.valid_keys.lower())
        valid_keys = set(strs.keys())
    else:
        valid_keys = set(data.split('\n'))

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

    if color:
        normal = NORMAL
        red = RED
        green = GREEN
        cyan = CYAN
    else:
        normal = red = green = cyan = ''

    status = 0
    sum_key_count = collections.defaultdict(int)
    print_names = len(opts.source) > 1
    for source in opts.source:
        if print_names:
            print("%s%s%s" % (cyan, source, normal))
        if opts.preproc:
            cmd = [opts.preproc]
            if opts.preproc_arg:
                cmd.extend(opts.preproc_arg)
            cmd.append(source)
            proc = subprocess.Popen(cmd, stdout=subprocess.PIPE)
            s = proc.stdout.read().decode()
            if proc.wait() != 0:
                return 1
            source = '<stdin>'

        elif source == '-':
            s = sys.stdin.read()
            source = '<stdin>'
        else:
            with open(source) as fp:
                s = fp.read()

        ok, key_count = validate_gettext(s, source, valid_keys,
                func_defs = func_defs,
                only_errors = opts.only_errors,
                before = opts.before,
                after = opts.after,
                color = color)

        for key, val in key_count.items():
            sum_key_count[key] += val

        if not ok:
            status = 1

    for key, val in sum_key_count.items():
        if val == 0:
            print("%sunused string:%s %s" % (red, normal, stringify(key)))

    return status

PO_LEX = re.compile(
    r'^([_a-zA-Z0-9][_a-zA-Z0-9]*)(?:\[(\d+|N)\])?\s*("(?:[^\\]|\\[\\nrtvabf"]|\\x[0-9a-fA-F]{2})*")\s*$|' # command
    r'^("(?:[^\\]|\\[\\nrtvabf"]|\\x[0-9a-fA-F]{2})*")\s*$|' # multiline continuation
    r'^(\s*)$|' # empty line
    r'^(#.*)$', # comment
    re.M
)

PO_COMMAND   = 1
PO_INDEX     = 2
PO_STR       = 3
PO_MULTI_STR = 4
PO_EMPTY     = 5
PO_COMMENT   = 6

def parse_po(s, filename):
    strs = {}
    msgid = None
    msgid_plural = None
    msgstr = {}
    key = None
    for i, line in enumerate(s.split("\n")):
        line = line.strip()
        m = PO_LEX.match(line)
        if not m:
            print("%s:%d: failed to parse line" % (filename, i + 1))
            print(line)
            continue
        
        cmd = m.group(PO_COMMAND)
        if cmd:
            if cmd == 'msgid':
                if msgid is not None:
                    strs[msgid] = (msgid_plural, msgstr)

                msgid = parse_string_token(m.group(PO_STR))[1]
                msgid_plural = None
                msgstr = {}

                key = 'msgid'
            elif cmd == 'msgid_plural':
                val = parse_string_token(m.group(PO_STR))[1]
                if msgid_plural is None:
                    msgid_plural = val
                else:
                    msgid_plural += val
                key = 'msgid_plural'
            elif cmd == 'msgstr':
                index = m.group(PO_INDEX)
                if index is None:
                    key = None
                elif index == 'N':
                    key = 'N'
                else:
                    key = int(index)
                msgstr[key] = parse_string_token(m.group(PO_STR))[1]
            elif cmd == 'msgctxt':
                pass
        elif m.group(PO_MULTI_STR):
            val = parse_string_token(m.group(PO_MULTI_STR))[1]
            if key == 'msgid':
                msgid += val
            elif key == 'msgid_plural':
                msgid_plural += val
            elif key == 'msgid_plural':
                msgid_plural += val
            elif key in msgstr:
                msgstr[key] += val
            else:
                msgstr[key] = val
        elif m.group(PO_COMMENT):
            pass
        elif m.group(PO_EMPTY):
            pass

    if msgid is not None:
        strs[msgid] = (msgid_plural, msgstr)
    
    return strs

if __name__ == '__main__':
    sys.exit(main(sys.argv[1:]))
