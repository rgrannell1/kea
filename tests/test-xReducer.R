
forall <- arrow:::forall
test_cases <- arrow:::test_cases

message("xReducer")

	forall(
		"reducing the empty collection is the empty collection",
		test_cases$collection_zero,
		xReducer(stop, coll) %equals% coll
	)

	forall(
		"reducing one value is that value",
		test_cases$num_positive_integer,
		xReducer(stop, num) %equals% num
	)

	forall(
		"Return( ) terminates immediately",
		test_cases$num_positive_integer,
		xReducer(
			function (acc, new) {

				if (acc == 1) Return (acc) else c(acc, new)

			},
			seq_len(num)) == 1
	)

	forall(
		"reducing plus is sum",
		test_cases$sum_over_integers,
		xReducer(fn, coll) %equals% sum(coll),
		given =
			length(coll) > 0
	)

message("arrow $ xReducer")

	forall(
		"collection $ xReducer",
		test_cases$sum_over_integers,
		x_(coll)$xReducer(fn)$x_() %equals% sum(coll),
		given =
			length(coll) > 0
	)

	forall(
		"collection $ xReducer",
		test_cases$sum_over_integers,
		x_(fn)$xReducer(coll)$x_() %equals% sum(coll),
		given =
			length(coll) > 0
	)

message("arrow $ xReducer")

	forall(
		"collection $ x_Reducer",
		test_cases$sum_over_integers,
		x_(coll)$x_Reducer(fn) %equals% sum(coll),
		given =
			length(coll) > 0
	)

	forall(
		"collection $ x_Reducer",
		test_cases$sum_over_integers,
		x_(fn)$x_Reducer(coll) %equals% sum(coll),
		given =
			length(coll) > 0
	)
