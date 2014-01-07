
forall <- arrow:::forall
test_cases <- arrow:::test_cases

message('xFormalsOf')

	forall(
		"nullary functions yield the empty list.",
		list(),
		xFormalsOf(function () {}) %equals% list()
	)

message('arrow $ xFormalsOf')

	forall(
		"nullary functions yield the empty list.",
		list(),
		x_(function () {})$xFormalsOf()$x_() %equals% list()
	)

message('arrow $ x_FormalsOf')

	forall(
		"nullary functions yield the empty list.",
		list(),
		x_(function () {})$x_FormalsOf() %equals% list()
	)