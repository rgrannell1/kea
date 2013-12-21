
forall <- arrow:::forall
test_cases <- arrow:::test_cases

message('xSplitString')

	forall(
		"splitting a character(0) is character(0)",
		test_cases$str_word,
		xSplitString(str, character(0)) %equals% character(0)
	)

	forall(
		"splitting an empty string is the empty string",
		test_cases$str_word,
		xSplitString(str, '') == ''
	)

	forall(
		"splitting an empty string is the empty string",
		test_cases$str_word,
		xSplitString(str, paste0('a', str)) == 'a',
	given =
		!('a' %in% strsplit('', str)[[1]])
	)

message('arrow $ xSplitString')

	forall(
		"splitting an empty string is the empty string",
		test_cases$str_word,
		x_(paste0('a', str))$xSplitString(str)$x() == 'a',
		given =
			!('a' %in% strsplit('', str)[[1]])
	)

	message('arrow $ x_SplitString')

		forall(
			"splitting an empty string is the empty string",
			test_cases$str_word,
			x_(paste0('a', str))$x_SplitString(str) == 'a',
		given =
			!('a' %in% strsplit('', str)[[1]])
		)
