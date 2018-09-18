#include <stdio.h>
#define FOO \
	_("this is ignored")

void _(void *key, ...) {}

void other() {
	return _("known");
}

void main() {
	const char *baz;
	_("foo\" bar \n", baz);

	::_("this " // comment
	// comment
				"xxx" /* comment */ "aaa"
			"should be found \0 \12 \u00a0 \U0000000A \t \x20 \r\n\v\b\f", a, b, c
	
	);

	puts("known");
	puts("unknown");

	puts(_(L"illegal" U"concatenation"));
	puts(_(u8"another" "illegal" L"concatenation"));

	puts(_("another" L" known " "key"));
	puts(_("key with umlauts äÖü"));
	puts(_(L"key with umlauts äÖü"));
	puts(_(u8"key with umlauts äÖü"));

	// _(egg)
	int i = 'f';
	++ i;
	i += 1;

	struct {} foo;
	foo._("this is ignored");

	(&foo)->_("so is this");


/* foo
 *
 * bar _("bacon")
 */
	int variable = 0;

_(variable);
			_();

	_("known");

	std::_("this should not");

	printf("%s\n", _("foo bar %c", '\0'));
}