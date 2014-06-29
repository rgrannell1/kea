
kiwi ::: load_test_dependencies(environment())


message("xMap (+)")

	over(coll) +

	describe('the empty collection always yield the empty collection') +
	holdsWhen(
		is_collection(coll) && length(coll) == 0,
		xMap(identity, coll) %is% list()
	) +

	describe('identity preserves contents') +
	holdsWhen(
		is_collection(coll),
		xMap(identity, coll) %is% as.list(coll)
	) +

	run()