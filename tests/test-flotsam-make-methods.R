
kea ::: load_test_dependencies()

message("is_variadic")

	is_variadic <- kea ::: is_variadic

	over(str) +

	describe('variadic methods always end in _') +
	holdsWhen(
		is.character(str) && length(str) == 1 && nchar(str) > 0,

		is.logical(is_variadic(str))
	) +

	describe('variadic methods always end in _') +
	holdsWhen(
		is.character(str) && length(str) == 1 &&
		nchar(str) > 0 &&
		tail(strsplit(str, '')[[1]], 1) == '_',

		is_variadic(str)
	) +

	describe('as_variadic') +
	holdsWhen(
		is.character(str) && length(str) == 1 && nchar(str) > 0,

		is_variadic(as_variadic(str))
	) +

	run(10)
