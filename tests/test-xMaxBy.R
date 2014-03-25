
forall <- arrow:::forall
test_cases <- arrow:::test_cases

require(arrow)

message("xMaxBy")

	forall(
		"max of one element in one element",
		test_cases $ collection,
		xMaxBy( xI, coll[1] ) %equals% coll[[1]],
		given =
			length(coll) > 0
	)

	forall(
		"maxby of numbers is the largest number",
		test_cases $ integers,
		xMaxBy( xI, coll ) == max(unlist(coll)),
		given =
			length(coll) > 0
	)
