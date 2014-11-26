
kea ::: load_test_dependencies(environment())

message("xRiffle")

	over(val, coll) +

	it('the empty collection is always empty') +
	holdsWhen(
		suchThat $ is_empty_collection(coll),

		xRiffle(val, coll) %is% list()
	) +

	it('the singleton list is unchanged') +
	holdsWhen(
		suchThat $ is_singleton_collection(coll),

		xRiffle(val, coll) %is% unname(as.list(coll))
	) +

	it('the length is 2 * length(coll) + 1') +
	holdsWhen(
		suchThat $ is_collection(coll) && length(coll) > 1,

		length(xRiffle(val, coll)) == (2 * length(coll)) - 1
	) +

	it('the odd positions are the value') +
	holdsWhen(
		suchThat $ is_collection(coll),

		{
			map <- xRiffle(val, coll)

			all( sapply(seq_along(map), function (ith) {

				ith %% 2 == 1 || identical( val, map[[ith]] )

			}) )

		}
	) +

	run()
