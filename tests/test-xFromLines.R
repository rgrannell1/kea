
forall <- kiwi:::forall
test_cases <- kiwi:::test_cases

require(kiwi)

message('xFromLines')

	forall(
		"xFromLines of character(0) is character(0)",
		list(),
		xFromLines(character(0)) %equals% character(0)
	)

	forall(
		"xFromLines of letters is pasted letters",
		test_cases$letters,
		xFromLines(coll) %equals% paste0(coll, collapse = '\n'),
		given =
			length(coll) > 0
	)
