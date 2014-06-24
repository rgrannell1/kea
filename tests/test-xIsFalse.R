
kiwi ::: load_test_dependencies(environment())
is_collection <- kiwi ::: is_collection

message("xIsFalse (+)")

	over(val) +

	describe('xIsFalse is true when the value is false.') +
	when(
		identical(val, False),
		xIsFalse(val)
	) +

	describe('xIsFalse is false when the value isnt') +
	when(
		!identical(val, False),
		!xIsFalse(val)
	) +

	run()
