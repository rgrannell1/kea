
forall <- kiwi:::forall
test_cases <- kiwi:::test_cases

require(kiwi)

message("xChunk")

	forall(
		"one divides into lists of one.",
		test_cases$collection,
		length(xChunk(1, coll)) == length(coll),
		given =
			length(coll) > 0
	)

	forall(
		"infinite doesn't divide collection.",
		test_cases$collection,
		xChunk(Inf, coll) %equals% list(as.list(coll)),
		given =
			length(coll) > 0
	)
