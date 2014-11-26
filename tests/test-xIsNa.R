
kea ::: load_test_dependencies(environment())

unit_test("xIsNa")

	over(val) +

	it('xIsNa is true when the value is Na.') +
	holdsWhen(
		suchThat $ is_na(val),

		xIsNa(val)
	) +

	it('xIsNa is false when the value isnt') +
	holdsWhen(
		suchThat $ not_na(val),

		!xIsNa(val)
	) +

	run()
