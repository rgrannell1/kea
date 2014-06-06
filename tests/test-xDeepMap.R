
forall <- kiwi:::forall
test_cases <- kiwi:::test_cases

require(kiwi)

message("xDeepMap")

	forall(
		"deepmap of an empty collection is list()",
		test_cases$collection_zero,
		xDeepMap('+', coll) %equals% list()
	)

	forall(
		"for flat collections mapping and deepmapping are the same",
		test_cases$num_positive_integer,
		xDeepMap(function (x) x + 1, 1:num) %equals% xMap(function (x) x + 1, 1:num)
	)
