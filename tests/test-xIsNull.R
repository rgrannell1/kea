
kea ::: load_test_dependencies(environment())

message("xIsNull")

	over(val) +

	it('xIsNull is true when the value is null.') +
	holdsWhen(
		suchThat $ is_null(val),

		xIsNull(val)
	) +

	it('xIsNull is false when the value isnt') +
	holdsWhen(
		suchThat $ not_null(val),

		!xIsNull(val)
	) +

	run()
