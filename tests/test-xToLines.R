
forall <- arrow:::forall
test_cases <- arrow:::test_cases

message('xToLines')

	forall(
		"lines of the empty string is an empty vector",
		list(),
		xToLines('') %equals% character(0)
	)

	forall(
		"xToLines of letters with newlines is pasted letters",
		test_cases$letters_with_newlines,
		{
			delim <- paste0(str, collapse = '')
			xToLines(paste0(coll, collapse = delim)) %equals% coll
		},
		given =
			length(coll) > 0 && length(str) > 0
	)

message('arrow $ xToLines')

	forall(
		"collection $ xToLines",
		list(),
		x_('')$xToLines()$x_() %equals% character(0)
	)

message('arrow $ x_ToLines')

	forall(
		"collection $ x_ToLines",
		list(),
		x_('')$x_ToLines() %equals% character(0)
)
