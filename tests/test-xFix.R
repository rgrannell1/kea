
forall <- arrow:::forall
test_cases <- arrow:::test_cases

require(arrow)

message('xFix')

forall(
	"partially applying no arguments is the original function.",
	test_cases$num_positive_integer,
	{
		f <- xFix(function (a, b) a, list())
		f(num, num + 1) == num
	}
)

