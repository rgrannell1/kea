
kiwi ::: load_test_dependencies(environment())

message("xReduce")

	over(coll) +

	describe("reducing the empty collection fails.") +
	failsWhen(
		is_collection(coll) && length(coll) == 0,
		xReduce('+', coll)
	) +

	describe("reducing a length-one collection is the first value.") +
	holdsWhen(
		is_collection(coll) && length(coll) == 1,
		xReduce(identity, coll) %is% coll[[1]]
	) +

	describe("Return( ) can terminate the computation") +
	holdsWhen(
		is_collection(coll) && length(coll) > 0,
		xReduce(function (...) Return(..1), coll) %is% coll[[1]]
	) +

	describe("Reduce right is last value in collection") +
	holdsWhen(
		is_collection(coll) && length(coll) > 0,
		xReduce(function (...) ..2, coll) %is% coll[[ length(coll) ]]
	) +

	run()
