
forall <- kiwi:::forall
test_cases <- kiwi:::test_cases

require(kiwi)

message('xFix')

	forall(
		"partially applying no arguments is the original function.",
		test_cases$num_positive_integer,
		{
			f <- xFix(function (a, b) a, list())
			f(num, num + 1) == num
		}
	)

