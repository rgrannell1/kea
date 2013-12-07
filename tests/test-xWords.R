
forall <- arrow:::forall
test_cases <- arrow:::test_cases

message('xWords')

	forall(
		"words of the empty string is an empty vector",
		list(),
		xWords('') %equals% character(0)
	)

	forall(
		"length of chars is nchars of a string",
		test_cases$str_word,
		length(xWords(str)) == nchar(str)
	)

message('arrow $ xWords')

	forall(
		"coll $ arrow",
		test_cases$str_word,
		length(x_(str)$xWords()$x()) == nchar(str)
	)
