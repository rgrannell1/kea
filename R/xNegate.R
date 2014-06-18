
#' xNegate
#'
#' Return the logical negation of a function.
#'
#' @section Type Signature:
#'     (any -> &lt;logical>) -> (any -> &lt;logical>)
#'
#' @details
#'
#'    \bold{xNegate} takes a predicate function - such as \bold{is.integer( )} -
#'    and returns the negation of that function - \bold{not.integer( )}.
#'
#'    \code{not.integer <- xNegate(is.integer)}
#'
#'    \code{is.integer(1L)}
#'
#'    \code{True}
#'
#'    \code{not.integer(1L)}
#'
#'    \code{False}
#'
#'    In general, if a function \bold{fn} returns true \bold{xNegate(fn)} returns false
#'    for the same value. Similarily, if \bold{fn} returns false then \bold{xNegate(fn)}
#'    returns true for that value. Finally (R uses three-value logic) if \bold{fn} returns
#'    na for a value so does its negation.
#'
#' @param
#'    pred a predicate. The predicate function to return a negation of.
#'
#' @return
#'    A predicate function with the same parametres as \code{pred}.
#'
#' @section Corner Cases:
#'
#' @family function_modifying_functions
#'
#'
#' @example
#'    inst/examples/example-xNegate.R
#'
#' @rdname xNegate
#' @export

xNegate <- MakeFun(function (pred) {

	do.call("function", list(
		as.pairlist(xFormalsOf(pred)),
		bquote({
			"A function created by xNegate."
			""
			!.(call_with_params("pred", pred))
		})
	))

})
