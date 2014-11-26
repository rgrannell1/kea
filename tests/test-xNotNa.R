
kea ::: load_test_dependencies(environment())

message("xNotNa")

	over(val) +

	it('xNotNa is true when the value is Na.') +
	holdsWhen(
		suchThat $ is_na(val),

		!xNotNa(val)
	) +

	it('xNotNa is false when the value isnt') +
	holdsWhen(
		suchThat $ not_na(val),

		xNotNa(val)
	) +

	run()
