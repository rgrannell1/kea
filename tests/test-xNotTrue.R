
kea ::: load_test_dependencies(environment())

message("xNotTrue")

	over(val) +

	describe('xNotTrue is false when the value is true.') +
	holdsWhen(
		isTRUE(val),
		!xNotTrue(val)
	) +

	describe('xNotTrue is false when the value isnt') +
	holdsWhen(
		!isTRUE(val),
		xNotTrue(val)
	) +

	run()
