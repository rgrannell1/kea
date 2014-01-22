
#' xArityOf
#'
#' Return the number of arguments a function can accept.
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

	insist$must_be_fn_matchable(fn, invoking_call)

	fn <- match_fn(fn)
	fn_params <- xParamsOf(fn)

	if ("..." %in% fn_params) {
		+Inf
	} else {
		length(fn_params)
	}
}
