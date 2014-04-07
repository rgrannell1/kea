
forall <- arrow:::forall
test_cases <- arrow:::test_cases

require(arrow)

message('Arrow Wildcard Lambda\'s')

	forall(
		"test that binary %% works",
		test_cases$num_integer,
		(x. %% num)(num) == 0,
		given =
			num != 0
	)

	forall(
		"test that binary %/% works",
		test_cases$num_integer,
		(x. %/% num)(num) == 1,
		given =
			num != 0
	)

	forall(
	    "test that binary ^ works",
	    test_cases$num_integer,
	    (x. ^ num)(num) == num ^ num
	)

	forall(
	    "test that binary ** works",
	    test_cases$num_integer,
	    (x. ** num)(num) == num ** num
	)

	forall(
		"test that binary * works",
		test_cases$num_integer,
		(x. * num)(num) == num^2
	)

	forall(
		"test that binary / works",
		test_cases$num_integer,
		(x. / num)(num) == 1,
		given =
			num != 0
	)

	forall(
		"test that binary + works",
		test_cases$num_integer,
		(x. + num)(num) == 2*num
	)

	forall(
		"test that binary - works",
		test_cases$num_integer,
		(x. - num)(num) == 0
	)


	forall(
		"test that unary + works",
		test_cases$num_integer,
		(+x.)(num) == num
	)

	forall(
		"test that unary - works",
		test_cases$num_integer,
		(-x.)(num) == -num
	)

	forall(
		"test that unary - works",
		test_cases$logical,
		(!x.)(coll) %equals% !coll
	)
