
forall <- arrow:::forall
test_cases <- arrow:::test_cases

require(arrow)


message('xPermute')

	forall(
		"empty list is identity",
		list(),
		xPermute(list(), list()) %equals% list()
	)

	forall(
		"permuting with seq along is identity",
		test_cases$collection,
		xPermute(seq_along(coll), list(coll)) %equals% list(as.list(coll)),
		given =
			length(coll) > 0
	)
