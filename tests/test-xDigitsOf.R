
forall <- arrow:::forall
test_cases <- arrow:::test_cases

message('xDigitsOf')

	forall(
		"digits of collection zero is zero",
		test_cases$collection_zero,
		xDigitsOf(0) == 0
	)

	forall(
		"digits of zero is zero",
		list(),
		xDigitsOf(0) == 0
	)
