
kea ::: load_test_dependencies(environment())

unit_test("xNotTrue")

	over(val) +

	it('xNotTrue is false when the value is true.') +
	holdsWhen(
		isTRUE(val),

		!xNotTrue(val)
	) +

	it('xNotTrue is false when the value isnt') +
	holdsWhen(
		!isTRUE(val),

		xNotTrue(val)
	) +

	run()
