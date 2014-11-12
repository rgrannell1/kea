
kea ::: load_test_dependencies(environment())

message("xChunk")

	over(coll) +

	describe("xChunking infinite times / length times creates one chunk") +
	holdsWhen(
		suchThat $ not_empty_collection(coll),

		length(xChunk(Inf,          coll)) == 1,
		length(xChunk(length(coll), coll)) == 1
	) +

	describe("xChunk once is identity") +
	holdsWhen(
		suchThat $ not_empty_collection(coll),

		length(xChunk(1, coll)) == length(coll)
	) +

	run()
