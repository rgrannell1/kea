
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

message("arrow $ xIterate")

	forall(
		"function $ xIterate",
		test_cases$num_positive_integer,
		x_(
			function (n) {
				if (n == num) Return(n) else n + 1
			})$xIterate(0)$x_() %equals% num,
		given =
			length(num) > 0
	)


message("arrow $ x_Iterate")

	forall(
		"function $ x_Iterate",
		test_cases$num_positive_integer,
		x_(
			function (n) {
				if (n == num) Return(n) else n + 1
			})$x_Iterate(0) %equals% num,
		given =
			length(num) > 0
	)
