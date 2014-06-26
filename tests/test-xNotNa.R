
kiwi ::: load_test_dependencies(environment())
is_collection <- kiwi ::: is_collection

message("xNotNa (+)")

	over(val) +

	describe('xNotNa is true when the value is Na.') +
	holdsWhen(
		is.na(val) && !is.nan(val) && length(val) == 1,
		!xNotNa(val)
	) +

	describe('xNotNa is false when the value isnt') +
	holdsWhen(
		!is.na(val) && !is.nan(val) && length(val) == 1,
		xNotNa(val)
	) +

	run()
