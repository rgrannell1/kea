
forall <- arrow:::forall
test_cases <- arrow:::test_cases

message('xToWords')

	forall(
		"words of the empty string is an empty vector",
		list(),
		xToWords('') %equals% character(0)
	)

	forall(
		"xToWords of letters with space is pasted letters",
		test_cases$letters_with_spaces,
		{
			delim <- paste0(str, collapse = '')
			xToWords(paste0(coll, collapse = delim)) %equals% coll
		},
		given =
			length(coll) > 0 && length(str) > 0
	)

message('arrow $ xToWords')

	forall(
		"collection $ xToWords",
		list(),
		x_('')$xToWords()$x_() %equals% character(0)
	)

message('arrow $ x_ToWords')

	forall(
		"collection $ x_ToWords",
		list(),
		x_('')$x_ToWords() %equals% character(0)
)
