
kea ::: load_test_dependencies(environment())

message("xIsNull")

	over(val) +

	describe('xIsNull is true when the value is null.') +
	holdsWhen(
		identical(val, Null),
		xIsNull(val)
	) +

	describe('xIsNull is false when the value isnt') +
	holdsWhen(
		!identical(val, Null),
		!xIsNull(val)
	) +

	run()
