
forall <- kiwi:::forall
test_cases <- kiwi:::test_cases

require(kiwi)

message("xMinBy")

	forall(
		"min of one element in one element",
		test_cases $ collection,
		xMinBy( xI, coll[1] ) %is% coll[[1]],
		given =
			length(coll) > 0
	)

	forall(
		"minby of numbers is the smallest number",
		test_cases $ integers,
		xMinBy( xI, coll ) == min(unlist(coll)),
		given =
			length(coll) > 0
	)
