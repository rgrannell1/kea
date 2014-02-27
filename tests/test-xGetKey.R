
forall <- arrow:::forall
test_cases <- arrow:::test_cases

require(arrow)


message("xGetKey")

	forall("getting from list with two identical keys returns both.",
		test_cases$str_word,
		xGetKey(str)(list()) %equals% NULL
	)

	forall("getting from list with two identical keys returns first.",
		test_cases$str_word,
		{
			coll <- structure(list(1, 2), names = c(str, str))
			xGetKey(str)(coll) %equals% 1
		},
		given =
			nchar(str) > 0
	)

	forall("only correct matches are returned.",
		test_cases$str_word,
		{
			coll <- structure(
				list(1, 2, 3),
				names = c(str, paste(str, '2'), str))

			xGetKey(str)(coll) %equals% 1
		},
		given =
			nchar(str) > 0
	)
