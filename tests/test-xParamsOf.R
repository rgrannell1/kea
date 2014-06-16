
forall <- kiwi:::forall
test_cases <- kiwi:::test_cases

require(kiwi)

message("xParamsOf")

	forall(
		"nullary functions yield the empty charvector.",
		list(),
		xParamsOf(function () {}) %is% character(0)
	)

	forall(
		"formals of normal functions is well behaved.",
		test_cases$base_function,
		xParamsOf(fn) %is% names(formals(fn)),
		given =
			length(formals(fn)) > 0
	)
