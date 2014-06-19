
kiwi ::: load_test_dependencies(environment())
is_collection <- kiwi ::: is_collection

message("xChunk (+)")

	over(coll) +

	describe("xChunking infinite times / length times creates one chunk") +
	when(
		is_collection(coll) && length(coll) > 0,
		xChunk(Inf,          coll) %is% list(as.list(coll)),
		xChunk(length(coll), coll) %is% list(as.list(coll))
	) +

	describe("xChunk once is identity") +
	when(
		is_collection(coll) && length(coll) > 0,
		xChunk(1, coll) %is% lapply(coll, list)
	) +

	run()
