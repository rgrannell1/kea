
forall <- arrow:::forall
test_cases <- arrow:::test_cases

require(arrow)


message("xIsVariadic")

	forall(
		"check that ... is in base parametres if is variadic.",
		test_cases$base_function,
		xIsVariadic(fn),
		given =
			'...' %in% names(formals(fn))
	)

	forall(
		"check that ... is in base parametres if is variadic.",
		test_cases$base_function,
		'...' %in% names(formals(fn)),
		given =
			xIsVariadic(fn)
	)
	forall(
		"check that ... isnt in base parametres if isnt variadic.",
		test_cases$base_function,
		'...' %!in% names(formals(fn)),
		given =
			!xIsVariadic(fn)
	)
