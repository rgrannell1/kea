
forall <- arrow:::forall
test_cases <- arrow:::test_cases

require(arrow)

message('xIsNot')

	forall(
		"na is na",
		list(),
		!xIsNot(Na, Na)
	)

	forall(
		"null is null",
		list(),
		!xIsNot(NULL, NULL)
	)

	forall(
		"nan is nan",
		list(),
		!xIsNot(NaN, NaN)
	)
