
kiwi ::: load_test_dependencies(environment())
is_collection <- kiwi ::: is_collection

message("xIsEmpty (+)")

	over(coll) +

	describe('xIsEmpty correctly reports lengths.') +
	holdsWhen(
		is_collection(coll),
		if (length(coll) == 0) xIsEmpty(coll) else !xIsEmpty(coll)
	) +

	run()
