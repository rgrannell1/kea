
kea ::: load_test_dependencies(environment())

message("xDeepMap")

	over(coll) +

	describe('deepmapping over empty list is empty list') +
	holdsWhen(
		is_collection(coll) && length(coll) == 0,

		xDeepMap(identity, coll) %is% keep_names(list(), coll)
	) +

	describe('deep-map any coll with identity is that coll') +
	holdsWhen(
		is_collection(coll),

		xDeepMap(identity, coll) %is% as.list(coll)
	) +

	run()
