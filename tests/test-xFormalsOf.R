
forall <- kiwi:::forall
test_cases <- kiwi:::test_cases

require(kiwi)

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
