
kea ::: load_test_dependencies(environment())

to_test       <- function (bool) {}
body(to_test) <- kea ::: Must_Be_Flag(bool, predSymbol)

message('Must_Be_Flag')

	over(bool) +

	describe('works for true, false, na') +
	worksWhen(
		is.logical(bool) && length(bool) == 1,
		to_test(bool)
	) +

	run()

	over(bool) +

	describe('fails for anything else.') +
	failsWhen(
		!is.logical(bool) || length(bool) != 1,
		to_test(bool)
	) +

	run()

