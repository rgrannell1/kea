
message('xFormals')

forall(
	"nullary functions yield the empty list.",
	list(),
	xFormals(function () {}) %equals% list()
)

forall(
	"formals work for non-primitive functions.",
	list(words = G$words()),
	{
		f <- function () {}
		formals(f) <- structure(words, names = words)

		names(xFormals(f)) %equals% words &&
		all(unlist(words) == words)
	},
	given =
		length(words)
)
