
kea ::: load_test_dependencies(environment())

unit_test("xRelate")

    over(coll) +

	it('the empty collection always yield the empty collection') +
	holdsWhen(
		and_(suchThat $ is_empty_collection, suchThat $ not_named)(coll),

		xRelate(identity, coll) %is% list()
	) +

	it('the empty collection always yield the empty collection (named)') +
	holdsWhen(
		and_(suchThat $ is_empty_collection, suchThat $ is_named)(coll),

		xRelate(identity, coll) %is% as_named(list())
	) +

	it('identity preserves contents') +
	holdsWhen(
		suchThat $ not_empty_collection(coll),

		xRelate(identity, coll) %is% lapply(coll, function (x) list(x, x))
	) +

	it('names are preserved') +
	holdsWhen(
		suchThat $ not_empty_collection(coll),

		names(xRelate(identity, coll)) %is% names(coll)
	) +

	it('operation returns 2-tuples') +
	holdsWhen(
		suchThat $ is_collection(coll),

		all(vapply(xRelate(identity, coll), length, integer(1)) == 2)
	) +

	it('identity repeats each input') +
	holdsWhen(
		suchThat $ is_collection(coll),

		all( vapply(xRelate(identity, coll), function (pair) {
			do.call(identical, pair)
		}, logical(1)) )
	) +

	run()
