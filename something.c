#include <stdio.h>
#define FOO \
	/* ignored here */ \
	_("but " \
	  "found " \
	  "where the macro is used")

const char *_(void *key, ...) { return NULL; _() }
const char *gettext(void *key) { return NULL; }

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
	printf("%s %d", FOO, 1, FOO);
	puts("known");
	puts(_(R"xx(
		
		unknown
)xx"));

	printf("%s %d %d %d", _(
#if 0
		"foo"),
#else
		"bar"),
#endif
		1, 2);
	puts(_(L"illegal" U"concatenation"));
	puts(_(u8"another" "illegal" L"concatenation"));

	puts(_("another" L" known " "key"));
	puts(gettext("key with umlauts äÖü"));
	puts(_(L"key with umlauts äÖü"));
	puts(_(u8"key with umlauts äÖü"));

	// this will be parsed as 4 arguments :/
	puts(_("known", std::pair<int, int>(1, 2), 123));

	// _(egg), _("bacon")
	int i = 'f';
	++ i;
	i += 1;

	struct {} foo;
	foo._("this is ignored");

	(&foo)->_("so is this");


/* foo
 *
 * bar _("spam")
 */
	int variable = 0;

_(variable);
			_();

	gettext("known");

	std::_("this should not");

	printf("%s\n", _("foo bar %c", '\0'));

	printf(_("foo %s", 1 + 2, 2, 3, (
		4 * 10
	), 5, "  foo  bar  ", func(1, 2, 3)));
}