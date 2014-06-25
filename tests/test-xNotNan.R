
kiwi ::: load_test_dependencies(environment())
is_collection <- kiwi ::: is_collection

message("xNotNan (+)")

	over(val) +

	describe('xNotNan is false when the value is NaN.') +
	holdsWhen(
		identical(val, NaN),
		!xNotNan(val)
	) +

	describe('xNotNan isnt NaN when the value is') +
	holdsWhen(
		!identical(val, NaN),
		xNotNan(val)
	) +

	run()
