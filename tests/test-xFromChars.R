
forall <- kiwi:::forall
test_cases <- kiwi:::test_cases

require(kiwi)

message('xFromChars')

	forall(
		"xFromChars of character(0) is character(0)",
		list(),
		xFromChars(character(0)) %is% character(0)
	)

	forall(
		"xFromChars of letters is pasted letters",
		test_cases$letters,
		xFromChars(coll) %is% paste0(coll, collapse = ''),
		given =
			length(coll) > 0
	)

