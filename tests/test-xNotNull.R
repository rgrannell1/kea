
kiwi ::: load_test_dependencies(environment())

message("xNotNull")

	over(val) +

	describe('xNotNull is true when the value is null.') +
	holdsWhen(
		identical(val, Null),
		!xNotNull(val)
	) +

	describe('xNotNull is false when the value isnt') +
	holdsWhen(
		!identical(val, Null),
		xNotNull(val)
	) +

	run()
