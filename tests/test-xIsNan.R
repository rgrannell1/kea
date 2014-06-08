
kiwi ::: load_test_dependencies(environment())
is_collection <- kiwi ::: is_collection

message("xIsNan (+)")

	over(val) +

	describe('xIsNan is true when the value is NaN.') +
	when(
		identical(val, NaN),
		xIsNan(val)
	) +

	describe('xIsNan is NaN when the value isnt') +
	when(
		!identical(val, NaN),
		!xIsNan(val)
	) +

	run()
