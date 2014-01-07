
forall <- arrow:::forall
test_cases <- arrow:::test_cases

message('xFromLines')

	forall(
		"xFromLines of character(0) is character(0)",
		list(),
		xFromLines(character(0)) %equals% character(0)
	)

	forall(
		"xFromLines of letters is pasted letters",
		test_cases$letters,
		xFromLines(coll) %equals% paste0(coll, collapse = '\n'),
		given =
			length(coll) > 0
	)

message('arrow $ xFromLines')

	forall(
		"xFromLines of letters is pasted letters",
		test_cases$letters,
		x_(coll)$xFromLines()$x_() %equals% paste0(coll, collapse = '\n'),
		given =
			length(coll) > 0
	)

message('arrow $ x_FromLines')

	forall(
		"x_FromLines of letters is pasted letters",
		test_cases$letters,
		x_(coll)$x_FromLines() %equals% paste0(coll, collapse = '\n'),
		given =
			length(coll) > 0
	)
