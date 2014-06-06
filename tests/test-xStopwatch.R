
forall <- kiwi:::forall
test_cases <- kiwi:::test_cases

require(kiwi)

message("xStopwatch")

	forall(
		"stopwatch of zero is false",
		list(),
		xStopwatch(0)() == False
	)

	forall(
		"stopwatch returns true before its time, then false",
		test_cases$num_one_to_ten,
		{

			num <- num / 100

			fn <- xStopwatch(num)
			start <- fn()
			Sys.sleep(num + 0.1)
			end <- fn()

			start && !end
		},
		max_time = 1
	)

