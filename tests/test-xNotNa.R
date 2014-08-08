
kea ::: load_test_dependencies(environment())

message("xNotNa")

	over(val) +

	describe('xNotNa is true when the value is Na.') +
	holdsWhen(
		is_atomic(val) && !is.na(val) && !is.nan(unlist(val)) && length(val) == 1,

		xNotNa(val)
	) +

	describe('xNotNa is false when the value isnt') +
	holdsWhen(
		is_atomic(val) && is.na(val) && !is.nan(unlist(val)) && length(val) == 1,

		!xNotNa(val)
	) +

	run()
