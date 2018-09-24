Validate Gettext
================

A very simple gettext validator for C/C++/Objective-C.

Example output:

![](https://i.imgur.com/QzsdxsA.png)

If you do fancy things with macros you might need to pass your source through
macro expansion first. This tool will be able to read the source line markers
and show you the issues in the actual source locations.

	 ./validate-gettext.py known_strings.txt -P gcc -A=-E -A=-I/include/path source.c

You can of course also pipe the output of the preprocessor into this script,
but letting the script run the preprocessor makes it possible for it to only
show you the string keys that aren't used in *any* of the source files, in
case you have multiple source files.

Note
----

This tries to only match calls to the global `_()` function. It can distinguish
between `_()`, `::_()`, `foo._()`, `foo->_()`, and `foo::_()` and will only
match the first two. However, it cannot detect if `_` is a local variable or a
class member, because that would require a full blown Objective-C++ parser. It
tries a bit to detect if its the `_()` is part of a function
declaration/definition, but again, you would need a full blown Objective-C++
parser to detect this (because you need to know if `foo` in `foo* _();` is
a type or a variable).

This doesn't correctly parse parameters that use C++ templates with multiple
template parameters. See:

    _("foo bar baz", std::pair<int, int>(1, 2), 123)
      ^- 1st arg     ^- 2nd arg     ^- 3rd arg  ^- 4th arg

However, if its enclosed in paranthesis it works:

    _("foo bar baz", (std::pair<int, int>(1, 2)), 123)
      ^- 1st arg     ^- 2nd arg                   ^- 3rd arg

A C++ parser would need to have type informations in order to even _parse_ such
code correctly. I won't write a full blown C++ parser. And for this particular
use we only need to parse the first argument and only if it is a string literal.

Usage
-----

	usage: validate-gettext.py [-h] [--func-defs FUNC_DEFS] [--only-errors]
	                           [--before LINES] [--after LINES]
	                           [--color {always,never,auto}]
	                           valid_keys source [source ...]
	
	positional arguments:
	  valid_keys            file with known gettext string keys, one key per line
	  source                C/C++/Objective-C source file
	
	optional arguments:
	  -h, --help            show this help message and exit
	  --func-defs FUNC_DEFS
	                        comma separated list of function definitions:
	                        <FUNC_NAME>[/<MIN_ARGC>-<MAX_ARGC>[/<KEY_INDEX>]] or
	                        <FUNC_NAME>[/<MIN_ARGC>-*[/<KEY_INDEX>]] or
	                        <FUNC_NAME>[/<ARGC>[/<KEY_INDEX>]]
	                        
	                        Example:
	                        validate-gettext.py --func-defs _/1/0,gettext/1/0 ...
	                        
	  --only-errors         show only errors, no valid gettext invokations
	  --before LINES        lines of context before a marked source location to show
	  --after LINES         lines of context after a marked source location to show
	  --color {always,never,auto}
	                        wether to colorize the output

TODO
----

Support ngettext (plural argument).

Support digraphs. Won't ever support trigraphs.

Support C++ custom literals. Currently they are parsed as a string/number
literal followed by an identifier, which is probably good enough for this?

MIT License
-----------

Copyright 2018 Mathias Panzenböck

Permission is hereby granted, free of charge, to any person obtaining a copy of
this software and associated documentation files (the "Software"), to deal in
the Software without restriction, including without limitation the rights to
use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
the Software, and to permit persons to whom the Software is furnished to do so,
subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
