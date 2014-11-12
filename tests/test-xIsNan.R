
kea ::: load_test_dependencies()

message("xIsNan")

	over(val) +

	describe('xIsNan is true when the value is NaN.') +
	holdsWhen(
		suchThat $ is_nan(val),

		xIsNan(val)
	) +

	describe('xIsNan is NaN when the value isnt') +
	holdsWhen(
		suchThat $ not_nan(val),

		!xIsNan(val)
	) +

	run()
