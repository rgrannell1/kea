
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




kiwi ::: load_test_dependencies(environment())
is_collection <- kiwi ::: is_collection

message("xChunk (+)")

	over(coll) +

	describe("xChunking infinite times / length times creates one chunk") +
	when(
		is_collection(coll) && length(coll) > 0,
		xChunk(Inf,          coll) %equals% list(as.list(coll)),
		xChunk(length(coll), coll) %equals% list(as.list(coll))
	) +

	describe("xChunk once is almost identity") +
	when(
		is_collection(coll) && length(coll) > 0,
		xChunk(1, coll) %equals% list(as.list(coll))
	) +

	run()
