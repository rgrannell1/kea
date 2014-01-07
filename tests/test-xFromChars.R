
forall <- arrow:::forall
test_cases <- arrow:::test_cases

message('xFromChars')

	forall(
		"xFromChars of character(0) is character(0)",
		list(),
		xFromChars(character(0)) %equals% character(0)
	)

	forall(
		"xFromChars of letters is pasted letters",
		test_cases$letters,
		xFromChars(coll) %equals% paste0(coll, collapse = ''),
		given =
			length(coll) > 0
	)

message('arrow $ xFromChars')

	forall(
		"xFromChars of letters is pasted letters",
		test_cases$letters,
		x_(coll)$xFromChars()$x_() %equals% paste0(coll, collapse = ''),
		given =
			length(coll) > 0
	)

message('arrow $ x_FromChars')

	forall(
		"x_FromChars of letters is pasted letters",
		test_cases$letters,
		x_(coll)$x_FromChars() %equals% paste0(coll, collapse = ''),
		given =
			length(coll) > 0
	)
