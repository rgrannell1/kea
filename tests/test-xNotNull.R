
kiwi ::: load_test_dependencies(environment())
is_collection <- kiwi ::: is_collection

message("xIsNull (+)")

	over(val) +

	describe('xIsNull is true when the value is null.') +
	when(
		identical(val, Null),
		xIsNull(val)
	) +

	describe('xIsNull is false when the value isnt') +
	when(
		!identical(val, Null),
		!xIsNull(val)
	) +

	run()
