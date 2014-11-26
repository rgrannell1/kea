
kea ::: load_test_dependencies(environment())

message("xRelate")

    over(coll) +

	describe('the empty collection always yield the empty collection') +
	holdsWhen(
		and_(suchThat $ is_empty_collection, suchThat $ not_named)(coll),

		xRelate(identity, coll) %is% list()
	) +

	describe('the empty collection always yield the empty collection (named)') +
	holdsWhen(
		and_(suchThat $ is_empty_collection, suchThat $ is_named)(coll),

		xRelate(identity, coll) %is% as_named(list())
	) +

	describe('identity preserves contents') +
	holdsWhen(
		suchThat $ not_empty_collection(coll),

		xRelate(identity, coll) %is% lapply(coll, function (x) list(x, x))
	) +

	describe('names are preserved') +
	holdsWhen(
		suchThat $ not_empty_collection(coll),

		names(xRelate(identity, coll)) %is% names(coll)
	) +

	describe('operation returns 2-tuples') +
	holdsWhen(
		suchThat $ is_collection(coll),

		all(vapply(xRelate(identity, coll), length, integer(1)) == 2)
	) +

	describe('identity repeats each input') +
	holdsWhen(
		suchThat $ is_collection(coll),

		all( vapply(xRelate(identity, coll), function (pair) {
			do.call(identical, pair)
		}, logical(1)) )
	) +

	run()
