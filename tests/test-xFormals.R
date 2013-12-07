
forall <- arrow:::forall
test_cases <- arrow:::test_cases

message('xFormals')

	forall(
		"nullary functions yield the empty list.",
		list(),
		xFormals(function () {}) %equals% list()
	)

	forall(
		"formals work for non-primitive functions.",
		test_cases$str_word,
		{
			f <- function () {}
			formals(f) <- structure(words, names = words)

			names(xFormals(f)) %equals% words &&
			all(unlist(words) == words)
		},
		given =
			length(words)
	)

message('arrow $ xFormals')

