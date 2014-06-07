
forall <- kiwi:::forall
test_cases <- kiwi:::test_cases

require(kiwi)

message("xIsVal")

	{
		xVal(a, 10)
		b <- 10
		stopifnot(xIsVal(a))
		stopifnot(!xIsVal(b))
	}
