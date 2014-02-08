
forall <- arrow:::forall
test_cases <- arrow:::test_cases

message("xVal")

	{
		xVal(a, 10)
		
		stopifnot(a == 10)
		stopifnot(
			bindingIsLocked(as.symbol('a'), environment()) )
	}
