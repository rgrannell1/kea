
forall <- arrow:::forall
test_cases <- arrow:::test_cases

require(arrow)


message('xSigni')

	forall(
		"less than zero numbers are -1",
		test_cases$num_integer,
		xSigni(num) == -1,
		given =
			num < 0
	)

	forall(
		"greater than zero numbers are -1",
		test_cases$num_integer,
		xSigni(num) == +1,
		given =
			num > 0
	)

	forall(
		"less than zero numbers are -1",
		test_cases$num_integer,
		xSigni(num) == 0,
		given =
			num == 0
	)
