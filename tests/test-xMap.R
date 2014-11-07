
kea ::: load_test_dependencies(environment())

message("xMap")

	over(coll) +

	describe('the empty collection always yield the empty collection') +
	holdsWhen(
		is_collection(coll) && length(coll) == 0 && !is_named(coll),

		xMap(identity, coll) %is% list()
	) +

	describe('the empty collection always yield the empty collection (named)') +
	holdsWhen(
		is_collection(coll) && length(coll) == 0 && is_named(coll),

		xMap(identity, coll) %is% as_named(list())
	) +

	describe('identity preserves contents') +
	holdsWhen(
		is_collection(coll) && length(coll) > 0,

		xMap(identity, coll) %is% as.list(coll)
	) +

	describe('names are preserved') +
	holdsWhen(
		is_collection(coll) && length(coll),

		names(xMap(identity, coll)) %is% names(coll),
		xMap(identity, names(coll)) %is% as.list(names(coll))
	) +

	run()
