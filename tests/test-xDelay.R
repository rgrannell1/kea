
kiwi ::: load_test_dependencies(environment())

require(kiwi)

message("xDelay")

	as_ratio <- function (num) {
		0.2 + (num / num^1.1)
	}

	over(num) +

	describe('stopwatch returns true before its time, then false') +
	holdsWhen(
		is.numeric(num) && length(num) == 1 && !is.infinite(num) &&
		!is.nan(num) && num > 0,
		{

			num <- as_ratio(num)
			!xDelay( xStopwatch(num), num + 0.1 )()
		},
		{

			num <- as_ratio(num)
			xDelay( xStopwatch(num), num - 0.1 )()
		}
	) +

	run()
