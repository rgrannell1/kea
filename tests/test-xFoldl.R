
forall <- arrow:::forall
test_cases <- arrow:::test_cases

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
		xFold(fn, 0, coll) %equals% sum(coll),
		given =
			length(coll) > 0
	)

message("arrow $ xFold")

	forall(
		"collection $ xFold",
		test_cases$sum_over_integers,
		x_(coll)$xFold(fn, 0)$x_() %equals% sum(coll),
		given =
			length(coll) > 0
	)

	forall(
		"collection $ xFold",
		test_cases$sum_over_integers,
		x_(fn)$xFold(0, coll)$x_() %equals% sum(coll),
		given =
			length(coll) > 0
	)

message("arrow $ x_Fold")

	forall(
		"collection $ x_Fold",
		test_cases$sum_over_integers,
		x_(coll)$x_Fold(fn, 0) %equals% sum(coll),
		given =
			length(coll) > 0
	)

	forall(
		"collection $ x_Fold",
		test_cases$sum_over_integers,
		x_(fn)$x_Fold(0, coll) %equals% sum(coll),
		given =
			length(coll) > 0
	)
