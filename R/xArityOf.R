
#' xArityOf
#'
#' Return the number of arguments a function can accept.
#'
#' @details
#'    \bold{xArityOf} returns the arity of a function - the number of
#'    arguments that can be passed to the function. R functions can be nullary
#'    (0-arguments), have infinitely many arguments (variadic functions), or
#'    have a fixed number of arguments (non-variadic functions).
#'
#' @param
#'    fn an function. The function to have its arity checked.
#'
#' @return
#'    A positive whole number.
#'
#' @section Corner Cases:
#'    If \bold{fn} has an ellipsis (...) parametre then \code{+Inf} is returned, as the
#'    function can accept an infinite number of arguments.
#'
#' @family parametre_functions
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

	insist $ must_not_be_missing(fn)

	insist $ must_be_fn_matchable(fn, invoking_call)

	fn <- match_fn(fn)
	fn_params <- xParamsOf(fn)

	if ("..." %in% fn_params) {
		+Inf
	} else {
		length(fn_params)
	}
}
