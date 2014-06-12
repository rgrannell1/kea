
kiwi ::: load_test_dependencies(environment())
is_collection <- kiwi ::: is_collection

message('xUnionOf (+)')

message("xUnionOf")

	over(coll) +

	describe('the union of an empty set is an empty set') +
	when(
		is_collection(coll) && length(coll) == 0,
		xUnionOf(coll) %equals% list()
	) +

	describe('the union of two equals sets is the first set') +
	when(
		is_collection(coll),
		xUnionOf_(unique(coll), unique(coll)) %equals% as.list(unique(coll))
	) +

	run()
