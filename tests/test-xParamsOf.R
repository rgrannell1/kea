
forall <- arrow:::forall
test_cases <- arrow:::test_cases

message("xParamsOf")

	forall(
		"nullary functions yield the empty charvector.",
		list(),
		xParamsOf(function () {}) %equals% character(0)
	)

	forall(
		"formals of normal functions is well behaved.",
		test_cases$base_function,
		xParamsOf(fn) %equals% names(formals(fn))
	)
