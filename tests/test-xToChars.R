
forall <- arrow:::forall
test_cases <- arrow:::test_cases

message('xToChars')

	forall(
		"chars of the empty string is an empty vector",
		list(),
		xToChars('') %equals% character(0)
	)

	forall(
		"chars of a single letter is identity",
		test_cases$letter,
		xToChars(str) == str
	)

	forall(
		"length of chars is nchars of a string",
		test_cases$str_word,
		length(xToChars(str)) == nchar(str)
	)

message('arrow $ xToChars')

	forall(
		"collection $ arrow",
		test_cases$str_word,
		length(x_(str)$xToChars()$x_()) == nchar(str)
	)

message('arrow $ x_ToChars')

	forall(
		"collection $ arrow",
		test_cases$str_word,
		length(x_(str)$x_ToChars()) == nchar(str)
	)

