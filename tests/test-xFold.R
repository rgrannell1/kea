kea ::: load_test_dependencies(environment())

message("xFold")

	over(val, coll) +

	describe("fold right is last value in collection") +
	holdsWhen(
		is_collection(coll) && length(coll) == 0,
		xFold(function (...) ..2, val, coll) %is% val
	) +


	describe("Return( ) can terminate the computation") +
	holdsWhen(
		is_collection(coll) && length(coll) > 0,
		xFold(function (...) Return(..2), val, coll) %is% coll[[ length(coll) ]]
	) +


	describe("Return( ) can terminate the computation") +
	holdsWhen(
		is_collection(coll) && length(coll) == 0,
		xFold(function (...) Return(..1), val, coll) %is% val
	) +

	describe("fold right is last value in collection") +
	holdsWhen(
		is_collection(coll) && length(coll) > 0,
		xFold(function (...) ..2, val, coll) %is% coll[[ length(coll) ]]
	) +

	run()
