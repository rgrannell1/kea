
kiwi ::: load_test_dependencies(environment())
is_collection <- kiwi ::: is_collection

message("xRestOf")

	over(coll) +

	describe("xRestOf of an empty collection yields the empty list") +
	holdsWhen(
		is_collection(coll) && length(coll) > 0,
		xRestOf(coll) %is% coll
	) +

	describe("xRestOf of an empty collection yields the empty list") +
	holdsWhen(
		is_collection(coll) && length(coll) > 0,
		length(xRestOf(coll)) == length(coll) - 1,
	) +

	run()