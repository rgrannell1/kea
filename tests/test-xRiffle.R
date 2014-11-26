
kea ::: load_test_dependencies(environment())

message("xRiffle")

	over(val, coll) +

	describe('the empty collection is always empty') +
	holdsWhen(
		suchThat $ is_empty_collection(coll),

		xRiffle(val, coll) %is% list()
	) +

	describe('the singleton list is unchanged') +
	holdsWhen(
		suchThat $ is_singleton_collection(coll),

		xRiffle(val, coll) %is% unname(as.list(coll))
	) +

	describe('the length is 2 * length(coll) + 1') +
	holdsWhen(
		suchThat $ is_collection(coll) && length(coll) > 1,

		length(xRiffle(val, coll)) == (2 * length(coll)) - 1
	) +

	describe('the odd positions are the value') +
	holdsWhen(
		suchThat $ is_collection(coll) && length(coll) > 1,

		{
			vals <- unname(xRiffle(val, coll)[seq_along(coll) %% 2 == 1])


			length(unique(vals)) == 1 && identical(unique(vals)[[1]], val)
		}
	) +

	run()
