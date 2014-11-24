
kea ::: load_test_dependencies(environment())

message("xIsNa")

	over(val) +

	describe('xIsNa is true when the value is Na.') +
	holdsWhen(
		suchThat $ is_na(val),

		xIsNa(val)
	) +

	describe('xIsNa is false when the value isnt') +
	holdsWhen(
		suchThat $ not_na(val),

		!xIsNa(val)
	) +

	run()
