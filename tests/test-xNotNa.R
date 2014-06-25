
kiwi ::: load_test_dependencies(environment())
is_collection <- kiwi ::: is_collection

message("xIsNa (+)")

	over(val) +

	describe('xIsNa is true when the value is Na.') +
	when(
		is.na(val),
		xIsNa(val)
	) +

	describe('xIsNa is false when the value isnt') +
	when(
		!is.na(val),
		!xIsNa(val)
	) +

	run()
