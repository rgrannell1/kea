
kiwi ::: load_test_dependencies(environment())


message("xIsTrue (+)")

	over(val) +

	describe('xIsTrue is true when the value is false.') +
	holdsWhen(
		isTRUE(val),
		xIsTrue(val)
	) +

	describe('xIsTrue is false when the value isnt') +
	holdsWhen(
		!isTRUE(val),
		!xIsTrue(val)
	) +

	run()
