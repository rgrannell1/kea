
forall <- kiwi:::forall
test_cases <- kiwi:::test_cases

require(kiwi)

message('xToWords')

	forall(
		"words of the empty string is an empty vector",
		list(),
		xToWords('') %is% character(0)
	)

	forall(
		"xToWords of letters with space is pasted letters",
		test_cases$letters_with_spaces,
		{
			delim <- paste0(str, collapse = '')
			xToWords(paste0(coll, collapse = delim)) %is% coll
		},
		given =
			length(coll) > 0 && length(str) > 0
	)

