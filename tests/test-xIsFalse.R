
kea ::: load_test_dependencies(environment())

unit_test("xIsFalse")

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




int_test("xIsFalse")

	over(val) +

	it('works with the constant logical predicates') +
	holdsWhen(
		True,

		!xIsFalse(xTruth(val)),
		xIsFalse(xFalsity(val)),
		!xIsFalse(xIrrelevance(val))
	) +

	run()
