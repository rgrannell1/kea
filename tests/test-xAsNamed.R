
forall <- arrow:::forall
test_cases <- arrow:::test_cases

message("xAsNamed")

	forall(
		"unit is named list()",
		test_cases$str_words,
		xAsNamed(character(0), list()) %equals% structure(list(), names = character(0))
	)

	forall(
		"same length names are correctly added",
		test_cases$str_words,
		all(names( xAsNamed(strs, seq_along(strs)) ) == strs)
	)

message("arrow $ xAsNamed")

	forall(
		"collection $ xAsNamed",
		test_cases$str_words,
		all(names( x_(seq_along(strs))$xAsNamed(strs)$x_() ) == strs)
	)

message("arrow $ x_AsNamed")

	forall(
		"collection $ x_AsNamed",
		test_cases$str_words,
		all(names( x_(seq_along(strs))$x_AsNamed(strs) ) == strs)
	)