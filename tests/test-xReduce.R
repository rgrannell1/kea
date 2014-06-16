
forall <- kiwi:::forall
test_cases <- kiwi:::test_cases

require(kiwi)

message("xReduce")

	forall(
		"reducing the empty collection is the empty collection",
		test_cases$collection_zero,
		xReduce(stop, coll) %is% coll
	)

	forall(
		"reducing one value is that value",
		test_cases$num_positive_integer,
		xReduce(stop, num) %is% num
	)

	forall(
		"Return( ) terminates immediately",
		test_cases$num_positive_integer,
		xReduce(
			function (acc, new) {

				if (acc == 1) Return (acc) else c(acc, new)

			},
			seq_len(num)) == 1
	)

	forall(
		"reducing plus is sum",
		test_cases$sum_over_integers,
		xReduce(fn, coll) %is% sum(coll),
		given =
			length(coll) > 0
	)
