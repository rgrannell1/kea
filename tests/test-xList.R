
kiwi ::: load_test_dependencies(environment())
is_collection <- kiwi ::: is_collection

message("xList (+)")

	over(coll) +

	describe("xList can construct empty lists") +
	when(
		is_collection(coll),
		xList[] %equals% list()
	) +

	describe('xList on one list is similar to as.list') +
	when(
		is_collection(coll),
		xList[x, x <- coll] %equals% as.list(coll)
	) +

	describe('predicates work with one binding') +
	when(
		is_collection(coll),
		xList[x, x <- coll, True]  %equals% as.list(coll)
	) +

	run()

message('xList (-)')

	over(coll) +

	describe('must have at least one binding') +
	failsWhen(
		is_collection(coll),
		xList[x],
		xList[x, y],
		xList[True]
	) +

	describe('must not being with binding expression') +
	failsWhen(
		is_collection(coll),
		xList[x <- coll, x],
		xList[x <- coll, y <- coll, x, y]
	) +

	run()
