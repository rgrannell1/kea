
kea ::: load_test_dependencies(environment())

require(kea)

message("xLimit")

	over(val, num) +

	describe("xLimit with zero is null function") +
	holdsWhen(
		True,

		is.null(xLimit(identity, 0)(val))
	) +

	describe("xLimit is null until it isnt") +
	holdsWhen(
		is_numeric(num) && length(num) == 1 &&
		!is.na(unlist(num)) && round(unlist(num)) == num && num > 0 && is.finite(unlist(num)) && num < 1000000,

		{

			res <- True
			limited <- xLimit(identity, num)

			for (ith in seq_len(num)) {
				res <- res && limited(val)
			}

			res && is.null(limited(val))
		}

	) +

	run()
