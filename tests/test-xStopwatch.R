
forall <- kiwi:::forall
test_cases <- kiwi:::test_cases

kiwi ::: load_test_dependencies(environment())
is_collection <- kiwi ::: is_collection

require(kiwi)

message("xStopwatch")

	over() +

	describe('stopwatch with no time is false') +
	holdsWhen(
		True,
		!xStopwatch(0)()
	) +

	run()

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






