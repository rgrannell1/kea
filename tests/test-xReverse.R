
kiwi ::: load_test_dependencies(environment())
is_collection <- kiwi ::: is_collection

message("xReverse (+)")

	over(coll) +

	describe("reversing the empty list is the empty collection") +
	holdsWhen(
		is_collection(coll) && length(coll) == 0
	) +

	describe("reversing a collection is the reversed collection") +
	holdsWhen(
		is_collection(coll),
		xReverse(coll) %is% as.list(rev(coll))
		xReverse(xReverse(coll)) %is% as.list(coll)

	) +

	run()
