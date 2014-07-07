
forall <- kiwi:::forall
test_cases <- kiwi:::test_cases

kiwi ::: load_test_dependencies(environment())

require(kiwi)

message("xStopwatch")

	over(num) +

	describe('stopwatch with no time is false') +
	holdsWhen(
		True,
		!xStopwatch(0)()
	) +

	describe('creating with any number always works') +
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

	describe('stopwatch returns true before its time, then false') +
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

	run(10)
