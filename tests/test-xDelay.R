
forall <- arrow:::forall
test_cases <- arrow:::test_cases

require(arrow)

message("xDelay")

	forall(
		"wait forces stopwatch to return false if held too long.",
		test_cases$num_one_to_ten,
		{

			num <- num / 100
			!xDelay( xStopwatch(num), num + 0.1 )()
		},
		max_time = 1
	)
