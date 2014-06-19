
forall <- kiwi:::forall
test_cases <- kiwi:::test_cases

require(kiwi)

message('xIsMatch')

	forall(
		"character 0 matches all strings",
		test_cases$str_word,
		xIsMatch(character(0), str) %is% logical(0)
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
		xIsMatch(str, character(0)) %is% logical(0)
	)
