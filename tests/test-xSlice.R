
forall <- arrow:::forall
test_cases <- arrow:::test_cases

require(arrow)

message("xSlice")

	forall(
		"selecting with integer zero is empty list",
		test_cases$collection_zero,
		xSlice(integer(0), coll) %equals% list()
	)
