
	kea ::: load_test_dependencies(environment())

	message("xIsNa")

		over(val) +

		describe('xIsNa is true when the value is Na.') +
		holdsWhen(
			is_atomic(val) && is.na(val) && !is.nan(unlist(val)) && length(val) == 1,

			xIsNa(val)
		) +

		describe('xIsNa is false when the value isnt') +
		holdsWhen(
			is_atomic(val) && !is.na(val) && !is.nan(unlist(val)) && length(val) == 1,

			!xIsNa(val)
		) +

		run()
