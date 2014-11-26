
kea ::: load_test_dependencies(environment())

unit_test("xNotNan")

	over(val) +

	it('xNotNan is false when the value is NaN.') +
	holdsWhen(
		suchThat $ is_nan(val),

		!xNotNan(val)
	) +

	it('xNotNan isnt NaN when the value is') +
	holdsWhen(
		suchThat $ not_nan(val),

		xNotNan(val)
	) +

	run()
