
forall <- kiwi:::forall
test_cases <- kiwi:::test_cases

require(kiwi)

message('xSliceString')

	forall(
		"selecting at zero returns the character(0).",
		list(),
		xSliceString('', 0) %equals% character(0)
	)

	forall(
		"selecting along the indices is identity",
		test_cases$str_word,
		xSliceString( str, seq_len(nchar(str)) ) %equals% str
	)

