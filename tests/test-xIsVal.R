
require(kea)

message("xIsVal")

	{
		xVal(a, 10)
		b <- 10
		stopifnot(xIsVal(a))
		stopifnot(!xIsVal(b))
	}
