
forall <- arrow:::forall
test_cases <- arrow:::test_cases

message('xUnwords')

	forall(
		"xUnwords of character(0) is character(0)",
		list(),
		xUnwords(character(0)) %equals% character(0)
	)

	forall(
		"xUnwords of letters is pasted letters",
		test_cases$letters,
		xUnwords(coll) %equals% paste0(coll, collapse = ' '),
		given =
			length(coll) > 0
	)

message('arrow $ xUnwords')

	forall(
		"xUnwords of letters is pasted letters",
		test_cases$letters,
		x_(coll)$xUnwords()$x() %equals% paste0(coll, collapse = ' '),
		given =
			length(coll) > 0
	)

message('arrow $ x_Unwords')

	forall(
		"x_Unwords of letters is pasted letters",
		test_cases$letters,
		x_(coll)$x_Unwords() %equals% paste0(coll, collapse = ' '),
		given =
			length(coll) > 0
	)
