
forall <- arrow:::forall
test_cases <- arrow:::test_cases

message('xSignum')

	forall(
		"less than zero numbers are -1",
		test_cases$num_integer,
		xSignum(num) == -1,
		given =
			num < 0
	)

	forall(
		"greater than zero numbers are -1",
		test_cases$num_integer,
		xSignum(num) == +1,
		given =
			num > 0
	)

	forall(
		"less than zero numbers are -1",
		test_cases$num_integer,
		xSignum(num) == 0,
		given =
			num == 0
	)

message('arrow $ xSignum')

	forall(
		"collection $ xSignum",
		test_cases$num_integer,
		x_(num)$xSignum()$x_() == -1,
		given =
			num < 0
	)

message('arrow $ x_Signum')

	forall(
		"collection $ x_Signum",
		test_cases$num_integer,
		x_(num)$x_Signum() == -1,
		given =
			num < 0
	)
