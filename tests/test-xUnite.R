
kea ::: load_test_dependencies(environment())

message('xUnite')

	over(coll) +

	describe('the union of an empty set is an empty set') +
	holdsWhen(
		is_collection(coll) && length(coll) == 0,

		xUnite(coll, coll) %is% list()
	) +

	describe('the union of two equals sets is the first set') +
	holdsWhen(
		is_collection(coll),

		xUnite(unique(coll), unique(coll)) %is% as.list(unique(coll))
	) +

	run()
