
kea ::: load_test_dependencies(environment())

message("xNotNan")

	over(val) +

	describe('xNotNan is false when the value is NaN.') +
	holdsWhen(
		suchThat $ is_nan(val),

		!xNotNan(val)
	) +

	describe('xNotNan isnt NaN when the value is') +
	holdsWhen(
		suchThat $ not_nan(val),

		xNotNan(val)
	) +

	run()
