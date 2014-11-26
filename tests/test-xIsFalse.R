
kea ::: load_test_dependencies(environment())

message("xIsFalse")

	over(val) +

	it('xIsFalse is true when the value is false.') +
	holdsWhen(
		suchThat $ is_false(val),

		xIsFalse(val)
	) +

	it('xIsFalse is false when the value isnt') +
	holdsWhen(
		suchThat $ not_false(val),

		!xIsFalse(val)
	) +

	run()
