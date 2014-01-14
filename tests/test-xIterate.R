
forall <- arrow:::forall
test_cases <- arrow:::test_cases

message("xIterate")

	forall(
		"Return is the identity function",
		test_cases$num_positive_integer,
		xIterate(function (n) Return(n), num) %equals% num
	)

	forall(
		"incrementing to a value works",
		test_cases$num_positive_integer,
		xIterate(
			function (n) {
				if (n == num) Return(n) else n + 1
			},
			0) %equals% num,
		given =
			length(num) > 0
	)

