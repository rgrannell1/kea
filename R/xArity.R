
#' xArity
#'
#' Return the arity of a function.
#'
#' @section Uses:
#' \code{xArity} is primarily intended for use with
#' higher-order functions that require functions with a
#' particular arity. For example, a strict version of
#' map might use \code{xArity} to throw an error if
#' its input function is not unary.
#'
#' @param fn an function of any arity.
#'
#' @return a positive whole number.
#'
#' @section Corner Cases:
#'	 If \code{fn} is a variadic function of any kind
#'	 positive infinity is returned.
#'
#' @family higher_order_functions
#'
#' @example inst/examples/blank.R
#' @export

xArity <- function (fn) {
	# Function -> integer
	# get the arity of a function.
	# returns +Inf if fn is a variadic function.

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
