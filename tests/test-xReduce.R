
kea ::: load_test_dependencies(environment())

message("xReduce")

	over(coll) +

	it("reducing the empty collection fails.") +
	failsWhen(
		suchThat $ is_empty_collection(coll),

		xReduce('+', coll)
	) +

	it("reducing a length-one collection is the first value.") +
	holdsWhen(
		suchThat $ is_collection(coll) && length(coll) == 1,

		xReduce(function (...) {}, coll) %is% coll[[1]]
	) +

	it("Return( ) can terminate the computation") +
	holdsWhen(
		suchThat $ not_empty_collection(coll),

		xReduce(function (...) Return(..1), coll) %is% coll[[1]]
	) +

	it("Reduce right is last value in collection") +
	holdsWhen(
		suchThat $ not_empty_collection(coll),

		xReduce(function (...) ..2, coll) %is% coll[[ length(coll) ]]
	) +

	run()
