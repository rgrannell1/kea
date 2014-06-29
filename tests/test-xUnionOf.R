
kiwi ::: load_test_dependencies(environment())


message('xUnionOf (+)')

	over(coll) +

	describe('the union of an empty set is an empty set') +
	holdsWhen(
		is_collection(coll) && length(coll) == 0,
		xUnionOf(coll) %is% list()
	) +

	describe('the union of two equals sets is the first set') +
	holdsWhen(
		is_collection(coll),
		xUnionOf_(unique(coll), unique(coll)) %is% as.list(unique(coll))
	) +

	run()
