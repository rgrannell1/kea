
forall <- kiwi:::forall
test_cases <- kiwi:::test_cases

require(kiwi)

message("xSlice")

	forall(
		"selecting with integer zero is empty list",
		test_cases$collection_zero,
		xSlice(integer(0), coll) %equals% list()
	)
