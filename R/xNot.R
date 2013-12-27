
#' xNot
#'
#' Return the logical negation of a function.
#'
#' @section Uses:
#'    \code{xNot} is useful when used alongside
#'    higher-order functions that takes a logical
#'    function as an input. For example, given a
#'    function to test if a value is an integer
#'    and the select function, it would be possible
#'    to select every value that is not an integer
#'    using \code{xNot}.
#'
#' @param
#'    pred a predicate of any arity.
#'
#' @return
#'    a predicate function of val.
#'
#' @family higher_order_functions
#'
#' @export

xNot <- function (pred) {
	# function -> function
	# negate a predicate function.

	invoking_call <- sys.call()

	assert(
		!missing(pred), sys.call(),
		exclaim$parametre_missing(pred))

	assert(
		is_fn_matchable(pred), invoking_call,
		exclaim$must_be_matchable(
			pred, profile_object(pred)) )

	remove(invoking_call)

	function (val) {
		"a boolean function returned by xNot( pred )"

		!pred(val)
	}
}
