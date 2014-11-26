
kea ::: load_test_dependencies(environment())

message("xIsNan")

	over(val) +

	it('xIsNan is true when the value is NaN.') +
	holdsWhen(
		suchThat $ is_nan(val),

		xIsNan(val)
	) +

	it('xIsNan is NaN when the value isnt') +
	holdsWhen(
		suchThat $ not_nan(val),

		!xIsNan(val)
	) +

	run()
