
kea ::: load_test_dependencies(environment())

unit_test("xNotNull")

	over(val) +

	it('xNotNull is true when the value is null.') +
	holdsWhen(
		suchThat $ is_null(val),

		!xNotNull(val)
	) +

	it('xNotNull is false when the value isnt') +
	holdsWhen(
		suchThat $ not_null(val),

		xNotNull(val)
	) +

	run()
