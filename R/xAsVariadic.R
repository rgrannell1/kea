
#' xAsVariadic
#'
#' Return a variadic function that passes its arguments
#' as a list to ints underyling function.
#'
#' @param
#'    fn a unary function.
#'
#' @return
#'    a variadic function.
#'
#' @family higher_order_functions
#'
#' @family function_modifying_functions
#'
#' @example
#'    inst/examples/example-xAsVariadic.R
#'
#' @export

xAsVariadic <- function (fn) {
	# (a -> b) -> (... -> b)
	# Return a variadic function that passes its arguments
	# as a list to its underlying function.

	invoking_call <- sys.call()

	assert(
		!missing(fn), invoking_call,
		exclaim$parametre_missing(fn))

	assert(
		is_fn_matchable(fn), invoking_call,
		exclaim$must_be_matchable(
			fn, profile_object(fn)) )

	fn <- match.fun(fn)

	function (...) {
		fn(list(...))
	}
}