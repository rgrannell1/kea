
forall <- arrow:::forall
test_cases <- arrow:::test_cases

require(arrow)

message("xIsVal")

	{
		xVal(a, 10)
		b <- 10
		stopifnot(xIsVal(a))
		stopifnot(!xIsVal(b))
	}
