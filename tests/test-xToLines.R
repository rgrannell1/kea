
forall <- kiwi:::forall
test_cases <- kiwi:::test_cases

require(kiwi)

message('xToLines')

	forall(
		"lines of the empty string is an empty vector",
		list(),
		xToLines('') %equals% character(0)
	)

	forall(
		"xToLines of letters with newlines is pasted letters",
		test_cases$letters_with_newlines,
		{
			delim <- paste0(str, collapse = '')
			xToLines(paste0(coll, collapse = delim)) %equals% coll
		},
		given =
			length(coll) > 0 && length(str) > 0
	)

