
forall <- arrow:::forall
test_cases <- arrow:::test_cases

require(arrow)

message("xRejectNan")

	forall(
		"removena of the empty collection is list()",
		test_cases$collection_zero,
		xRejectNan(coll) %equals% list()
	)

	forall(
		"rejecting a list of nans is the empty list",
		test_cases$num_positive_integer,
		xRejectNan(rep(list(NaN), num)) %equals% list()
	)
