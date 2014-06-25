
kiwi ::: load_test_dependencies(environment())
is_collection <- kiwi ::: is_collection

message("xAt (+)")

	over(coll) +

	describe("xAt an index is the same as [[ index") +
	holdsWhen(
		is_collection(coll) && length(coll) > 0,
		{
			ind <- sample(seq_along(coll), size = 1)
			xAt(ind, coll) %is% coll[[ind]]
		}
	) +

	run()

message('xAt (-)')

	over(coll) +

	describe('fails when index is too large') +
	failsWhen(
		is_collection(coll) && length(coll) >= 1,
		xAt(length(coll) + 1, coll)
	) +

	describe('fails when index is too small') +
	failsWhen(
		is_collection(coll) && length(coll) >= 1,
		xAt(0, coll)
	) +

	run()
