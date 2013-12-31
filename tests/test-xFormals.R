
forall <- arrow:::forall
test_cases <- arrow:::test_cases

message('xFormals')

	forall(
		"nullary functions yield the empty list.",
		list(),
		xFormals(function () {}) %equals% list()
	)

message('arrow $ xFormals')

	forall(
		"nullary functions yield the empty list.",
		list(),
		x_(function () {})$xFormals()$x_() %equals% list()
	)

message('arrow $ x_Formals')

	forall(
		"nullary functions yield the empty list.",
		list(),
		x_(function () {})$x_Formals() %equals% list()
	)