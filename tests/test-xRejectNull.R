
forall <- arrow:::forall
test_cases <- arrow:::test_cases

require(arrow)

message("xRejectNull")

	forall(
		"remove null of the empty collection is list()",
		test_cases$collection_zero,
		xRejectNull(coll) %equals% list()
	)

	forall(
		"rejecting a list of nulls is the empty list",
		test_cases$num_positive_integer,
		xRejectNull(rep(list(Null), num)) %equals% list()
	)
