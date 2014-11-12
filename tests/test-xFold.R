kea ::: load_test_dependencies(environment())

message("xFold")

	over(val, coll) +

	describe("fold right is last value in collection") +
	holdsWhen(
		suchThat $ is_empty_collection(coll),

		xFold(function (...) ..2, val, coll) %is% val
	) +

	describe("Return( ) can terminate the computation") +
	holdsWhen(
		suchThat $ not_empty_collection(coll),

		xFold(function (...) Return(..2), val, coll) %is% coll[[ 1 ]]
	) +

	describe("Return( ) can terminate the computation") +
	holdsWhen(
		suchThat $ is_empty_collection(coll),

		xFold(function (...) Return(..1), val, coll) %is% val
	) +

	describe("fold right is last value in collection") +
	holdsWhen(
		suchThat $ not_empty_collection(coll),

		xFold(function (...) ..2, val, coll) %is% coll[[ length(coll) ]]
	) +

	run()
