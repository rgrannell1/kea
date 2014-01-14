
forall <- arrow:::forall
test_cases <- arrow:::test_cases

message('xFormalsOf')

	forall(
		"nullary functions yield the empty list.",
		list(),
		xFormalsOf(function () {}) %equals% list()
	)
