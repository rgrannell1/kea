
kiwi ::: load_test_dependencies(environment())

message("xIsNa")

	over(val) +

	describe('xIsNa is true when the value is Na.') +
	holdsWhen(
		is.na(val) && !is.nan(val) && length(val) == 1,
		xIsNa(val)
	) +

	describe('xIsNa is false when the value isnt') +
	holdsWhen(
		!is.na(val) && !is.nan(val) && length(val) == 1,
		!xIsNa(val)
	) +

	run()
