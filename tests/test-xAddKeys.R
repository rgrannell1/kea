
forall <- kiwi:::forall
test_cases <- kiwi:::test_cases

require(kiwi)

message("xAddKeys")

	forall(
		"unit is named list()",
		test_cases$str_words,
		xAddKeys(character(0), list()) %is% structure(list(), names = character(0))
	)

	forall(
		"same length names are correctly added",
		test_cases$str_words,
		all(names( xAddKeys(strs, seq_along(strs)) ) == strs)
	)

