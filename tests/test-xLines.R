
forall <- arrow:::forall
test_cases <- arrow:::test_cases

message('xLines')

	forall(
		"lines of the empty string is an empty vector",
		list(),
		xLines('') %equals% character(0)
	)

	forall(
		"xLines of letters with newlines is pasted letters",
		test_cases$letters_with_newlines,
		{
			delim <- paste0(str, collapse = '')
			xLines(paste0(coll, collapse = delim)) %equals% coll
		},
		given =
			length(coll) > 0 && length(str) > 0
	)

message('arrow $ xLines')

	forall(
		"collection $ xLines",
		list(),
		x_('')$xLines()$x_() %equals% character(0)
	)

message('arrow $ x_Lines')

	forall(
		"collection $ x_Lines",
		list(),
		x_('')$x_Lines() %equals% character(0)
)
