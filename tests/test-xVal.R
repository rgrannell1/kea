
forall <- kiwi:::forall
test_cases <- kiwi:::test_cases

require(kiwi)

message("xVal")

	{
		xVal(a, 10)
		stopifnot(a == 10)
		stopifnot(bindingIsLocked(
			as.symbol('a'), environment()) )

		unlockBinding(as.symbol('a'), environment())
		rm(a)
	}
