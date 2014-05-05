
forall <- arrow:::forall
test_cases <- arrow:::test_cases

require(arrow)

message('xIsMatch')

	forall(
		"character 0 matches all strings",
		test_cases$str_word,
		xIsMatch(character(0), str) == True
	)

	forall(
		"'' matches all strings",
		test_cases$str_word,
		xIsMatch('', str) == True
	)

	forall(
		"str matches all strings",
		test_cases$str_word,
		xIsMatch(str, str) == True
	)

	forall(
		"str never matches empty string",
		test_cases$str_word,
		xIsMatch(str, character(0)) %equals% logical(0)
	)
