
forall <- arrow:::forall
test_cases <- arrow:::test_cases

message('xExplode')

	forall(
		"splitting a character(0) is character(0)",
		test_cases$str_word,
		xExplode(str, character(0)) %equals% character(0)
	)

	forall(
		"splitting an empty string is the empty string",
		test_cases$str_word,
		xExplode(str, '') == ''
	)

	forall(
		"splitting an empty string is the empty string",
		test_cases$str_word,
		xExplode(str, paste0('a', str)) == 'a',
	given =
		!('a' %in% strsplit('', str)[[1]])
	)

message('arrow $ xExplode')

	forall(
		"splitting an empty string is the empty string",
		test_cases$str_word,
		x_(paste0('a', str))$xExplode(str)$x_() == 'a',
		given =
			!('a' %in% strsplit('', str)[[1]])
	)

	message('arrow $ x_Explode')

		forall(
			"splitting an empty string is the empty string",
			test_cases$str_word,
			x_(paste0('a', str))$x_Explode(str) == 'a',
		given =
			!('a' %in% strsplit('', str)[[1]])
		)