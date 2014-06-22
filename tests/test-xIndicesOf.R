
kiwi ::: load_test_dependencies(environment())
is_collection <- kiwi ::: is_collection

message("xIndicesOf (+)")

	over(coll) +

	describe("the empty collection always yields the empty integer vector") +
	when(
		length(coll) == 0 && is_collection(coll),
		xIndicesOf(coll) %is% integer(0)
	) +

	describe("the upper index is the length of the collection") +
	when(
		length(coll) > 0 && is_collection(coll),
		max(xIndicesOf(coll)) == length(coll)
	) +


	run()
