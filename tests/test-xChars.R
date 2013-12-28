
forall <- arrow:::forall
test_cases <- arrow:::test_cases

message('xChars')

	forall(
		"chars of the empty string is an empty vector",
		list(),
		xChars('') %equals% character(0)
	)

	forall(
		"chars of a single letter is identity",
		test_cases$letter,
		xChars(str) == str
	)

	forall(
		"length of chars is nchars of a string",
		test_cases$str_word,
		length(xChars(str)) == nchar(str)
	)

message('arrow $ xChars')

	forall(
		"collection $ arrow",
		test_cases$str_word,
		length(x_(str)$xChars()$x()) == nchar(str)
	)

message('arrow $ x_Chars')

	forall(
		"collection $ arrow",
		test_cases$str_word,
		length(x_(str)$x_Chars()) == nchar(str)
	)

