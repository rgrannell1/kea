
kiwi ::: load_test_dependencies(environment())
is_collection <- kiwi ::: is_collection

message("xIsFalse (+)")

	over(val) +

	describe('xIsFalse is true when the value is false.') +
	holdsWhen(
		identical(val, False),
		xIsFalse(val)
	) +

	describe('xIsFalse is false when the value isnt') +
	holdsWhen(
		!identical(val, False),
		!xIsFalse(val)
	) +

	run()
