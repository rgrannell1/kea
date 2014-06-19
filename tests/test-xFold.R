
forall <- kiwi:::forall
test_cases <- kiwi:::test_cases

require(kiwi)

message("xFold")

	forall(
		"folding the empty collection is the initial element",
		test_cases$collection_zero,
		xFold(stop, 1, coll) == 1
	)

	forall(
		"folding one value is fn of the value and the initial element",
		test_cases$num_positive_integer,
		xFold('+', num, num) == num + num
	)

	forall(
		"Return( ) terminates immediately",
		test_cases$num_positive_integer,
		xFold(
			function (acc, new) {

				if (new == 1) Return (new) else c(acc, new)

			},
			0,
			seq_len(num)) == 1
	)

	forall(
		"folding plus is sum",
		test_cases$sum_over_integers,
		xFold(fn, 0, coll) %is% sum(coll),
		given =
			length(coll) > 0
	)
