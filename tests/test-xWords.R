
forall <- arrow:::forall
test_cases <- arrow:::test_cases

message('xWords')

	forall(
		"words of the empty string is an empty vector",
		list(),
		xWords('') %equals% character(0)
	)

	forall(
		"xWords of letters with space is pasted letters",
		test_cases$letters_with_spaces,
		{
			delim <- paste0(str, collapse = '')
			xWords(paste0(coll, collapse = delim)) %equals% coll
		},
		given =
			length(coll) > 0 && length(str) > 0
	)

message('arrow $ xWords')

	forall(
		"collection $ xWords",
		list(),
		x_('')$xWords()$x_() %equals% character(0)
	)

message('arrow $ x_Words')

	forall(
		"collection $ x_Words",
		list(),
		x_('')$x_Words() %equals% character(0)
)
