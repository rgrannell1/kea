
forall <- kiwi:::forall
test_cases <- kiwi:::test_cases

require(kiwi)

message('xFromWords')

	forall(
		"xFromWords of character(0) is character(0)",
		list(),
		xFromWords(character(0)) %is% character(0)
	)

	forall(
		"xFromWords of letters is pasted letters",
		test_cases$letters,
		xFromWords(coll) %is% paste0(coll, collapse = ' '),
		given =
			length(coll) > 0
	)
