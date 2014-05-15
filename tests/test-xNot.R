
forall <- arrow:::forall
test_cases <- arrow:::test_cases

require(arrow)

message('xNot')

	forall(
		"na is na",
		list(),
		!xNot(Na, Na)
	)

	forall(
		"null is null",
		list(),
		!xNot(NULL, NULL)
	)

	forall(
		"nan is nan",
		list(),
		!xNot(NaN, NaN)
	)
