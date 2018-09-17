Validate Gettext
================

A very simple gettext validator for C/C++/Objective-C. I wrote this in a couple
of hours on two days.

Example output:

![](https://i.imgur.com/gbimISU.png)

Note
----

This tries to only match calls to the global `_()` function. It can distinguish
between `_()`, `::_()`, `foo._()`, `foo->_()`, and `foo::_()` and will only
match the first two. However, it cannot detect if `_` is a local variable or a
member, because that would require a full blown C++ parser.

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

TODO
----

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
