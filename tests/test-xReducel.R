
forall <- arrow:::forall
test_cases <- arrow:::test_cases

message("xReduce")

	forall(
		"reducing the empty collection is the empty collection",
		test_cases$collection_zero,
		xReduce(stop, coll) %equals% coll
	)

	forall(
		"reducing one value is that value",
		test_cases$num_positive_integer,
		xReduce(stop, num) %equals% num
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
		xReduce(fn, coll) %equals% sum(coll),
		given =
			length(coll) > 0
	)

message("arrow $ xReduce")

	forall(
		"collection $ xReduce",
		test_cases$sum_over_integers,
		x_(coll)$xReduce(fn)$x() %equals% sum(coll),
		given =
			length(coll) > 0
	)

	forall(
		"collection $ xReduce",
		test_cases$sum_over_integers,
		x_(fn)$xReduce(coll)$x() %equals% sum(coll),
		given =
			length(coll) > 0
	)

message("arrow $ x_Reduce")

	forall(
		"collection $ x_Reduce",
		test_cases$sum_over_integers,
		x_(coll)$x_Reduce(fn) %equals% sum(coll),
		given =
			length(coll) > 0
	)

	forall(
		"collection $ x_Reduce",
		test_cases$sum_over_integers,
		x_(fn)$x_Reduce(coll) %equals% sum(coll),
		given =
			length(coll) > 0
	)
