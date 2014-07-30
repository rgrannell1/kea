
kiwi ::: load_test_dependencies(environment())

message("xIsEmpty")

	over(coll) +

	describe('xIsEmpty correctly reports lengths.') +
	holdsWhen(
		is_collection(coll),

		if (length(coll) == 0) xIsEmpty(coll) else !xIsEmpty(coll)
	) +

	run()
