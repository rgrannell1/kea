
forall <- arrow:::forall
test_cases <- arrow:::test_cases

message("xName")

	forall(
		"unit is named list()",
		test_cases$str_words,
		xName(character(0), list()) %equals% structure(list(), names = character(0))
	)

	forall(
		"same length names are correctly added",
		test_cases$str_words,
		all(names( xName(strs, seq_along(strs)) ) == strs)
	)

message("arrow $ xName")

	forall(
		"collection $ xName",
		test_cases$str_words,
		all(names( x_(seq_along(strs))$xName(strs)$x_() ) == strs)
	)

message("arrow $ x_Name")

	forall(
		"collection $ x_Name",
		test_cases$str_words,
		all(names( x_(seq_along(strs))$x_Name(strs) ) == strs)
	)