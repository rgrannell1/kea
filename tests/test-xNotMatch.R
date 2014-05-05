
forall <- arrow:::forall
test_cases <- arrow:::test_cases

require(arrow)

message('xNotMatch')

	forall(
		"character 0 never matches all strings",
		test_cases$str_word,
		xNotMatch(character(0), str) == False
	)

	forall(
		"'' never matches all strings",
		test_cases$str_word,
		xNotMatch('', str) == False
	)

	forall(
		"str never matches all strings",
		test_cases$str_word,
		xNotMatch(str, str) == False
	)

	forall(
		"str never matches empty string",
		test_cases$str_word,
		xNotMatch(str, character(0)) %equals% logical(0)
	)

