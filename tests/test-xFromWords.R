
forall <- arrow:::forall
test_cases <- arrow:::test_cases

message('xFromWords')

	forall(
		"xFromWords of character(0) is character(0)",
		list(),
		xFromWords(character(0)) %equals% character(0)
	)

	forall(
		"xFromWords of letters is pasted letters",
		test_cases$letters,
		xFromWords(coll) %equals% paste0(coll, collapse = ' '),
		given =
			length(coll) > 0
	)

message('arrow $ xFromWords')

	forall(
		"xFromWords of letters is pasted letters",
		test_cases$letters,
		x_(coll)$xFromWords()$x_() %equals% paste0(coll, collapse = ' '),
		given =
			length(coll) > 0
	)

message('arrow $ x_FromWords')

	forall(
		"x_FromWords of letters is pasted letters",
		test_cases$letters,
		x_(coll)$x_FromWords() %equals% paste0(coll, collapse = ' '),
		given =
			length(coll) > 0
	)
