
kea ::: load_test_dependencies(environment())

require(kea)

message("xStopwatch")

	over(num) +

	it('stopwatch with no time is false') +
	holdsWhen(
		True,

		!xStopwatch(0)()
	) +

	it('creating with any number always works') +
	worksWhen(
		is.numeric(num) && length(num) == 1 && !is.infinite(num) &&
		!is.nan(num) && num > 0,

		xStopwatch(num)
	) +

	run()

	as_ratio <- function (num) {
		0.2 + (num / num^1.1)
	}

	over(num) +

	it('stopwatch returns true before its time, then false') +
	holdsWhen(
		is.numeric(num) && length(num) == 1 && !is.infinite(num) &&
		!is.nan(num) && num > 0,

		{

			num        <- as_ratio(num)
			timer      <- xStopwatch(num)

			startValue <- timer()
				Sys.sleep(num + 0.1)
			endValue   <- timer()

			startValue && !endValue
		}
	) +

	run(5)
