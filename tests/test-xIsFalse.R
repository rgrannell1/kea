
kea ::: load_test_dependencies()

message("xIsFalse")

	over(val) +

	describe('xIsFalse is true when the value is false.') +
	holdsWhen(
		suchThat $ is_false(val),

		xIsFalse(val)
	) +

	describe('xIsFalse is false when the value isnt') +
	holdsWhen(
		suchThat $ not_false(val),

		!xIsFalse(val)
	) +

	run()
