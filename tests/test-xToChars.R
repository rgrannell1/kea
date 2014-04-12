
forall <- arrow:::forall
test_cases <- arrow:::test_cases

require(arrow)

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

