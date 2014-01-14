
forall <- arrow:::forall
test_cases <- arrow:::test_cases

message('xSubstring')

	forall(
		"selecting at zero returns the character(0).",
		list(),
		xSubstring('', 0) %equals% character(0)
	)

	forall(
		"selecting along the indices is identity",
		test_cases$str_word,
		xSubstring( str, seq_len(nchar(str)) ) %equals% str
	)

message('arrow $ xSubstring')

	forall(
		"collection $ xSubstring",
		test_cases$str_word,
		x_(str)$xSubstring( seq_len(nchar(str)) )$x_() %equals% str
	)

message('arrow $ x_Substring')

	forall(
		"collection $ x_Substring",
		test_cases$str_word,
		x_(str)$x_Substring( seq_len(nchar(str)) ) %equals% str
	)

