
kea ::: load_test_dependencies(environment())

unit_test("xNotFalse")

	over(val) +

	it('xNotFalse of false is false.') +
	holdsWhen(
		suchThat $ is_false(val),

		!xNotFalse(val)
	) +

	it('xNotFalse of values is true.') +
	holdsWhen(
		suchThat $ not_false(val),

		xNotFalse(val)
	) +

	it('xNotFalse is always true or false') +
	holdsWhen(
		TRUE,

		xNotFalse(val) || !xNotFalse(val)
	) +

	run()
