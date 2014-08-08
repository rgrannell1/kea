
require(kea)

message("xVal")

	{
		xVal(test_value, 10)
		stopifnot(test_value == 10)
		stopifnot(bindingIsLocked(
			as.symbol('test_value'), environment()) )

		unlockBinding(as.symbol('test_value'), environment())
		rm(test_value)
	}
