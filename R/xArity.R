
#' xArity
#'
#' Return the number of arguments a function can accept.
#'
#' @section Uses:
#'    \code{xArity} is primarily intended for use with
#'    higher-order functions that require functions with a
#'    particular arity. For example, a strict version of
#'    map might use \code{xArity} to throw an error if
#'    its input function is not unary.
#'
#' @param
#'    fn an function of any arity.
#'
#' @return
#'    a positive whole number.
#'
#' @section Corner Cases:
#'    If \code{fn} has an ellipsis (...) parameter then \code{+Inf} is returned, as the
#'    function can accept an infinite number of arguments.
#'
#' @family
#'    higher_order_functions
#'
#' @export

xArity <- function (fn) {
	# function -> integer
	# get the arity of a function.

	invoking_call <- sys.call()

	assert(
		!missing(fn), invoking_call,
		exclaim$parameter_missing(fn))

	fn <- dearrowise(fn)

	assert(
		is_fn_matchable(fn), invoking_call,
		exclaim$must_be_matchable(fn))

	fn <- match.fun(fn)
	fn_params <- names(xFormals(fn))

	if ("..." %in% fn_params) {
		+Inf
	} else {
		length(fn_params)
	}
}
