
forall <- arrow:::forall
test_cases <- arrow:::test_cases

message("xStopWatch")

	forall(
		"stopwatch of zero is false",
		list(),
		xStopWatch(0)() == False
	)

	forall(
		"stopwatch returns true before its time, then false",
		test_cases$num_one_to_ten,
		{

			num <- num / 100

			fn <- xStopWatch(num)
			start <- fn()
			Sys.sleep(num + 0.1)
			end <- fn()

			start && !end
		},
		max_time = 1
	)

