
kiwi ::: load_test_dependencies(environment())
is_collection <- kiwi ::: is_collection

message("xChunk (+)")

	over(coll) +

	describe("xChunking infinite times / length times creates one chunk") +
	holdsWhen(
		is_collection(coll) && length(coll) > 0,
		length(xChunk(Inf,          coll)) == 1,
		length(xChunk(length(coll), coll)) == 1
	) +

	describe("xChunk once is identity") +
	holdsWhen(
		is_collection(coll) && length(coll) > 0,
		length(xChunk(1, coll)) == length(coll)
	) +

	run()
