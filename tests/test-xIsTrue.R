
kiwi ::: load_test_dependencies(environment())
is_collection <- kiwi ::: is_collection

message("xIsTrue (+)")

	over(val) +

	describe('xIsTrue is true when the value is false.') +
	when(
		isTRUE(val),
		xIsTrue(val)
	) +

	describe('xIsTrue is false when the value isnt') +
	when(
		!isTRUE(val),
		!xIsTrue(val)
	) +

	run()
