
forall <- arrow:::forall
test_cases <- arrow:::test_cases

message('xFormalsOf')

	forall(
		"nullary functions yield the empty list.",
		list(),
		xFormalsOf(function () {}) %equals% list()
	)

	forall(
		"formals of normal functions is well behaved.",
		test_cases$base_function,
		xFormalsOf(fn) %equals% as.list(formals(fn))
	)
