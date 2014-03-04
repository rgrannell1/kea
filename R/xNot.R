
#' xNot
#'
#' Return the logical negation of a function.
#'
#' @details
#'
#'    \bold{xNot} takes a predicate function - such as \bold{is.integer( )} -
#'    and returns the negation of that function - \bold{not.integer( )}.
#'
#'    \code{not.integer <- xNot(is.integer)}
#'
#'    \code{is.integer(1L)}
#'
#'    \code{True}
#'
#'    \code{not.integer(1L)}
#'
#'    \code{False}
#'
#'    In general, if a function \bold{fn} returns true \bold{xNot(fn)} returns false
#'    for the same value. Similarily, if \bold{fn} returns false then \bold{xNot(fn)}
#'    returns true for that value. Finally (R uses three-value logic) if \bold{fn} returns
#'    na for a value so does its negation.
#'
#' @param
#'    pred a predicate. The predicate function to return a negation of.
#'
#' @return
#'    A predicate function with the same parametres as \code{pred}.
#'
#' @family function_modifying_functions
#'
#' @example
#'    inst/examples/example-xNot.R
#'
#' @rdname xNot
#' @export

xNot <- MakeFun(function (pred) {
	# function -> function
	# negate a predicate function.

	invoking_call <- sys.call()

	MACRO( arrow ::: Must $ Not_Be_Missing(pred) )

	MACRO( arrow ::: Must $ Be_Fn_Matchable(pred) )

	remove(invoking_call)

	do.call("function", list(
		as.pairlist(xFormalsOf(pred)),
		bquote({
			"A function created by xNot."
			""
			!.(call_with_params("pred", pred))
		})
	))

})
