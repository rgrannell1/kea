
forall <- arrow:::forall
test_cases <- arrow:::test_cases

require(arrow)

message("xAddKeys")

	forall(
		"unit is named list()",
		test_cases$str_words,
		xAddKeys(character(0), list()) %equals% structure(list(), names = character(0))
	)

	forall(
		"same length names are correctly added",
		test_cases$str_words,
		all(names( xAddKeys(strs, seq_along(strs)) ) == strs)
	)

