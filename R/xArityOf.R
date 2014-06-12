
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

	fn_params <- if (is.primitive(fn)) {
		# -- use the args function to get the primitive arguments.
		names(as.list( head(as.list(args(fn)), -1) ))
	} else {
		names(as.list( formals(fn) ))
	}

	if (any(fn_params == '...')) {
		+Inf
	} else {
		as.numeric(length(fn_params))
	}
})
