
kea ::: load_test_dependencies(environment())

message("xIsTrue")

	over(val) +

	it('xIsTrue is true when the value is false.') +
	holdsWhen(
		suchThat $ is_true(val),

		xIsTrue(val)
	) +

	it('xIsTrue is false when the value isnt') +
	holdsWhen(
		suchThat $ is_false(val),

		!xIsTrue(val)
	) +

	run()
