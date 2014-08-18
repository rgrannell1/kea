
kea ::: load_test_dependencies(environment())

message("xRiffle")

	over(val, coll) +

	describe('the empty collection is always empty') +
	holdsWhen(
		is_collection(coll) && length(coll) == 0,
		xRiffle(val, coll) %is% list()
	) +

	describe('the singleton list is unchanged') +
	holdsWhen(
		is_collection(coll) && length(coll) == 1,
		xRiffle(val, coll) %is% unname(as.list(coll))
	) +

	describe('the length is 2*length(coll) + 1') +
	holdsWhen(
		is_collection(coll) && length(coll) > 1,
		length(xRiffle(val, coll)) == (2 * length(coll)) - 1
	) +

	run()
