
kea ::: load_test_dependencies()

message("xIsTrue")

	over(val) +

	describe('xIsTrue is true when the value is false.') +
	holdsWhen(
		suchThat $ is_true(val),

		xIsTrue(val)
	) +

	describe('xIsTrue is false when the value isnt') +
	holdsWhen(
		suchThat $ is_false(val),

		!xIsTrue(val)
	) +

	run()
