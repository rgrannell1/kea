
forall <- arrow:::forall
test_cases <- arrow:::test_cases

require(arrow)

message("xScan")

	forall(
		"scan with the empty list is list of val.",
		test_cases$num_integer,
		xScan("+",0, list()) %equals% list(val)
	)
