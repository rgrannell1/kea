
kiwi ::: load_test_dependencies(environment())

message("xDeepMap")

	over(coll) +

	describe('deepmapping over empty list is empty list') +
	holdsWhen(
		is_collection(coll) && length(coll) == 0,
		xDeepMap(identity, coll) %is% list()
	) +

	run()
