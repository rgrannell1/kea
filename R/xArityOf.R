
#' xArityOf
#'
#' Return the number of arguments a function can accept.
#'
#' @section Uses:
#'    \code{xArityOf} is primarily intended for use with
#'    higher-order functions that require functions with a
#'    particular arity. For example, a strict version of
#'    map might use \code{xArityOf} to throw an error if
#'    its input function is not unary.
#'
#' @param
#'    fn an function of any arity.
#'
#' @return
#'    A positive whole number.
#'
#' @section Corner Cases:
#'    If \code{fn} has an ellipsis (...) parametre then \code{+Inf} is returned, as the
#'    function can accept an infinite number of arguments.
#'
#' @example
#'    inst/examples/example-xArityOf.R
#'
#' @rdname xArityOf
#' @export

xArityOf <- function (fn) {
	# function -> integer
	# get the arity of a function.

	invoking_call <- sys.call()

	assert(
		!missing(fn), invoking_call,
		exclaim$parametre_missing(fn))

	assert(
		is_fn_matchable(fn), invoking_call,
		exclaim$must_be_matchable(
			fn, summate(fn)) )

	fn <- match_fn(fn)
	fn_params <- names(xFormalsOf(fn))

	if ("..." %in% fn_params) {
		+Inf
	} else {
		length(fn_params)
	}
}
