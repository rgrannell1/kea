
kea ::: load_test_dependencies(environment())

unit_test("is_variadic")

	is_variadic <- kea ::: is_variadic

	over(str) +

	it('variadic methods always end in _') +
	holdsWhen(
		is.character(str) && length(str) == 1 && nchar(str) > 0,

		is.logical(is_variadic(str))
	) +

	it('variadic methods always end in _') +
	holdsWhen(
		is.character(str) && length(str) == 1 &&
		nchar(str) > 0 &&
		tail(strsplit(str, '')[[1]], 1) == '_',

		is_variadic(str)
	) +

	it('as_variadic') +
	holdsWhen(
		is.character(str) && length(str) == 1 && nchar(str) > 0,

		is_variadic(as_variadic(str))
	) +

	run(10)
