
kea ::: load_test_dependencies()

message("xIsNull")

	over(val) +

	describe('xIsNull is true when the value is null.') +
	holdsWhen(
		suchThat $ is_null(val),

		xIsNull(val)
	) +

	describe('xIsNull is false when the value isnt') +
	holdsWhen(
		suchThat $ not_null(val),

		!xIsNull(val)
	) +

	run()
