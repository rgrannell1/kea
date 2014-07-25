
kiwi ::: load_test_dependencies(environment())

message("xIndicesOf")

	over(coll) +

	describe("the empty collection always yields the empty integer vector") +
	holdsWhen(
		length(coll) == 0 && is_collection(coll),
		xIndicesOf(coll) %is% integer(0)
	) +

	describe("the upper index is the length of the collection") +
	holdsWhen(
		length(coll) > 0 && is_collection(coll),
		max(xIndicesOf(coll)) == length(coll)
	) +

	run()
