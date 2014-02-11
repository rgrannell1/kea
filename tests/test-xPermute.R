
forall <- arrow:::forall
test_cases <- arrow:::test_cases

require(arrow)


message('xPermute')

	forall(
		"empty list is identity",
		list(),
		xPermute(list(), list()) %equals% list()
	)
