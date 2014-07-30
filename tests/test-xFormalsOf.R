
forall <- kiwi:::forall
test_cases <- kiwi:::test_cases

require(kiwi)

message('xFormalsOf')

	forall(
		"nullary functions yield the empty list.",
		list(),
		xFormalsOf(function () {}) %is% list()
	)

	forall(
		"formals of normal functions is well behaved.",
		test_cases $ base_function,
		xFormalsOf(fn) %is% as.list(formals(fn))
	)
