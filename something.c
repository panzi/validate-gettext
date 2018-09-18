#include <stdio.h>
#define FOO \
	_("this is ignored")

void main() {
	const char *baz;
	_("foo\" bar \n", baz);

	::_("this " // comment
	// comment
				"xxx" /* comment */ "aaa"
			"should be found \0 \12 \u00a0 \U0000000A \t \x20 \r\n\v\b\f", a, b, c
	
	);

	// _(egg)
	int i = 'f';
	++ i;
	i += 1;

	struct Foo foo;
	foo._("this is ignored");

	(&foo)->_("so is this");


/* foo
 *
 * bar _("bacon")
 */

_(variable);
			_();

	_("known");

	std::_("this should not");

	printf("%s\n", _("foo bar %c", '\0'));
}