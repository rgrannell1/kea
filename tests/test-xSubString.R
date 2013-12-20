
forall <- arrow:::forall
test_cases <- arrow:::test_cases

message('xSubString')

	forall(
		"selecting at zero returns the character(0).",
		list(),
		xSubString('', 0) %equals% character(0)
	)

	forall(
		"selecting along the indices is identity",
		test_cases$str_word,
		xSubString( str, seq_len(nchar(str)) ) %equals% str
	)

	forall(
		"selecting along the indices is identity",
		test_cases$str_word,
		xSubString(str, rev( seq_len(nchar(str)) )) %equals% str
	)

message('arrow $ xSubString')

	forall(
		"collection $ xSubString",
		test_cases$str_word,
		x_(str)$xSubString( seq_len(nchar(str)) )$x() %equals% str
	)

message('arrow $ x_SubString')

	forall(
		"collection $ x_SubString",
		test_cases$str_word,
		x_(str)$x_SubString( seq_len(nchar(str)) ) %equals% str
	)

