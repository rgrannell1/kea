
forall <- arrow:::forall
test_cases <- arrow:::test_cases

require(arrow)

message("xMinBy")

	forall(
		"min of one element in one element",
		test_cases $ collection,
		xMinBy( xI, coll[1] ) %equals% coll[[1]],
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
