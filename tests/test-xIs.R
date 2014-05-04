
forall <- arrow:::forall
test_cases <- arrow:::test_cases

require(arrow)

message('xIs')

	forall(
		"na is na",
		list(),
		xIs(Na, Na)
	)

	forall(
		"null is null",
		list(),
		xIs(NULL, NULL)
	)

	forall(
		"nan is nan",
		list(),
		xIs(NaN, NaN)
	)
