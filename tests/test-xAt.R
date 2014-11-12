
kea ::: load_test_dependencies(environment())

message("xAt")

	over(coll) +

	describe("xAt an index is the same as [[ index") +
	holdsWhen(
		suchThat $ not_empty_collection(coll),

		{
			ind <- sample(seq_along(coll), size = 1)
			xAt(ind, coll) %is% coll[[ind]]
		}
	) +

	run()

message('xAt')

	over(coll) +

	describe('fails when index is too large') +
	failsWhen(
		suchThat $ is_collection(coll) && length(coll) >= 1,

		xAt(length(coll)       + 1, coll),
		xAt(list(length(coll)) + 1, coll)
	) +

	describe('fails when index is too small') +
	failsWhen(
		suchThat $ is_collection(coll) && length(coll) >= 1,

		xAt(0, coll)
	) +

	run()
