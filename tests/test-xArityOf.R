
forall <- arrow:::forall
test_cases <- arrow:::test_cases

require(arrow)


message('xArityOf')

	forall(
		"the arity of the null function is null",
		list(),
		xArityOf(function () Null) == 0
	)

	forall(
		"the arity of the primitive functions is defined",
		test_cases$base_primitive,
		xArityOf(fn) >= 0
	)

	forall(
		"the arity of the non-primitive functions is defined",
		test_cases$base_function,
		xArityOf(fn) >= 0
	)

