
#' xAsVariadic
#'
#' Return a variadic function that passes its arguments as a list to ints underyling function.
#'
#' @param fn a unary function.
#'
#' @return a variadic function.
#'

#' @export

xAsVariadic <- function (fn) {
	# (a -> b) -> (... -> b)
	# Return a variadic function that passes its arguments
	# as a list to its underlying function.

	invoking_call <- sys.call()

	assert(
		!missing(fn), invoking_call,
		exclaim$parameter_missing(fn))

	fn <- dearrowise(fn)

	assert(
		is_fn_matchable(fn), invoking_call,
		exclaim$must_be_matchable(fn))

	fn <- match.fun(fn)

	function (...) {
		fn(list(...))
	}
}