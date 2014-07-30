
require(kiwi)

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
		round(num) == num && num > 0 && !is.na(num) && is.finite(num) && num < 1000000,
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
