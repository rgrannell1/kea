
kea ::: load_test_dependencies(environment())

message("xRelate")

	over(coll) +

	describe('the empty collection always yield the empty collection') +
	holdsWhen(
		is_collection(coll) && length(coll) == 0 && !is_named(coll),
		xRelate(identity, coll) %is% list()
	) +

	describe('the empty collection always yield the empty collection (named)') +
	holdsWhen(
		is_collection(coll) && length(coll) == 0 && is_named(coll),
		xRelate(identity, coll) %is% as_named(list())
	) +

	describe('identity preserves contents') +
	holdsWhen(
		is_collection(coll) && length(coll) > 0,
		xRelate(identity, coll) %is% lapply(coll, function (x) list(x, x))
	) +

	describe('names are preserved') +
	holdsWhen(
		is_collection(coll) && length(coll),

		names(xRelate(identity, coll)) %is% names(coll),
		xRelate(identity, names(coll)) %is% as.list(names(coll))
	) +

	run()
