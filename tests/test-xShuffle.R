
forall <- kiwi:::forall
test_cases <- kiwi:::test_cases

require(kiwi)

message("xShuffle")

	forall(
		"shuffling the empty collection returns the empty list",
		test_cases$collection_zero,
		xShuffle(coll) %equals% list()
	)

	forall(
		"shuffling preserves length",
		test_cases$collection,
		length(xShuffle(coll)) == length(coll)
	)

	forall(
		"shuffling returns a list",
		test_cases$collection,
		is.list(xShuffle(coll))
	)
