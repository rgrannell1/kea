
forall <- arrow:::forall
test_cases <- arrow:::test_cases

message('xArity')

	forall(
		"the arity of the null function is null",
		list(),
		xArity(function () Null) == 0
	)

	forall(
		"the arity of the primitive functions is defined",
		test_cases$base_primitive,
		xArity(fn) >= 0
	)

	forall(
		"the arity of the non-primitive functions is defined",
		test_cases$base_function,
		xArity(fn) >= 0
	)

message('arrow $ xArity')

message('fn $ xArity')

	forall(
		"the arity of the non-primitive functions is defined",
		test_cases$base_function,
		x_(fn)$xArity()$x() >= 0
	)




