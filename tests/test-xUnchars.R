
forall <- arrow:::forall
test_cases <- arrow:::test_cases

message('xUnchars')

	forall(
		"xUnchars of character(0) is character(0)",
		list(),
		xUnchars(character(0)) %equals% character(0)
	)

	forall(
		"xUnchars of letters is pasted letters",
		test_cases$letters,
		xUnchars(coll) %equals% paste0(coll, collapse = ''),
		given =
			length(coll) > 0
	)

message('arrow $ xUnchars')

	forall(
		"xUnchars of letters is pasted letters",
		test_cases$letters,
		x_(coll)$xUnchars()$x() %equals% paste0(coll, collapse = ''),
		given =
			length(coll) > 0
	)

message('arrow $ x_Unchars')

	forall(
		"x_Unchars of letters is pasted letters",
		test_cases$letters,
		x_(coll)$x_Unchars() %equals% paste0(coll, collapse = ''),
		given =
			length(coll) > 0
	)
