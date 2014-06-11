
#' xArityOf
#'
#' Return the number of arguments a function can accept.
#'
#' @section Type Signature:
#'     function -> &lt;numeric>
#'
#' @details
#'    \bold{xArityOf} returns the arity of a function - the number of
#'    arguments that can be passed to the function. R functions can be nullary
#'    (zero-arguments), have infinitely many arguments (variadic functions), or
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

xArityOf <- MakeFun(function (fn) {

	MACRO( Must $ Be_Fn_Matchable(fn) )

	fn <- match_fn(fn)
	fn_params <- xParamsOf(fn)

	if ("..." %in% fn_params) {
		+Inf
	} else {
		as.numeric(length(fn_params))
	}
})
