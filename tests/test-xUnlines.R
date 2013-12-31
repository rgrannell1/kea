
forall <- arrow:::forall
test_cases <- arrow:::test_cases

message('xUnlines')

	forall(
		"xUnlines of character(0) is character(0)",
		list(),
		xUnlines(character(0)) %equals% character(0)
	)

	forall(
		"xUnlines of letters is pasted letters",
		test_cases$letters,
		xUnlines(coll) %equals% paste0(coll, collapse = '\n'),
		given =
			length(coll) > 0
	)

message('arrow $ xUnlines')

	forall(
		"xUnlines of letters is pasted letters",
		test_cases$letters,
		x_(coll)$xUnlines()$x_() %equals% paste0(coll, collapse = '\n'),
		given =
			length(coll) > 0
	)

message('arrow $ x_Unlines')

	forall(
		"x_Unlines of letters is pasted letters",
		test_cases$letters,
		x_(coll)$x_Unlines() %equals% paste0(coll, collapse = '\n'),
		given =
			length(coll) > 0
	)
