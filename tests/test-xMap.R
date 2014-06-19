
kiwi ::: load_test_dependencies(environment())
is_collection <- kiwi ::: is_collection

message("xMap (+)")

	over(coll) +

	describe('the empty collection always yield the empty collection') +
	when(
		is_collection(coll) && length(coll) == 0,
		xMap(identity, coll) %is% list()
	) +

	describe('identity preserves contents') +
	when(
		is_collection(coll),
		xMap(identity, coll) %is% as.list(coll)
	) +

	run()