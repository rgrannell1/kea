
forall <- arrow:::forall
test_cases <- arrow:::test_cases

require(arrow)

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
