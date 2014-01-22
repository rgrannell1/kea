
#' xNot
#'
#' Return the logical negation of a function.
#'
#' @param
#'    pred a predicate of any arity.
#'
#' @return
#'    A predicate function of val.
#'
#' @family function_modifying_functions
#'
#' @rdname xNot
#' @export

xNot <- function (pred) {
	# function -> function
	# negate a predicate function.

	invoking_call <- sys.call()

	assert(
		!missing(pred), sys.call(),
		exclaim$parametre_missing(pred))

	insist$must_be_fn_matchable(pred, invoking_call)

	remove(invoking_call)

	function (val) {
		"a boolean function returned by xNot."
		""
		!pred(val)
	}
}
