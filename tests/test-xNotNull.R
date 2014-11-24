
kea ::: load_test_dependencies(environment())

message("xNotNull")

	over(val) +

	describe('xNotNull is true when the value is null.') +
	holdsWhen(
		suchThat $ is_null(val),

		!xNotNull(val)
	) +

	describe('xNotNull is false when the value isnt') +
	holdsWhen(
		suchThat $ not_null(val),

		xNotNull(val)
	) +

	run()
