
#' xConst
#'
#' Create a function that returns a particular value.
#'
#' @section Uses:
#'    \code{xConst} is useful when used alongside the lift family
#'    of functions. For example, the \code{xModLift} function
#'    could be partially applied with \code{xConst(6)} as its left
#'    argument to create a \code{xModLift6} function, that modulo-6s
#'    the output of its underlying function. \code{xConst} is also
#'    useful for creating function analogues to TRUE, FALSE and NA
#'    for use with arrow functions that take a predicate.
#'
#' @param
#'    val an arbitrary value.
#'
#' @return
#'    a variadic function.
#'
#' @family
#'    higher_order_functions
#'
#' @export

xConst <- function (val) {
	# return a function that closes over the variable val.

	assert(
		!missing(val), sys.call(),
		exclaim$parametre_missing(val))

	function (...) {
		"a function created xConst."
		""
		val
	}
}

#' @export

xKestrel <- xConst

#' @export

xK <- xKestrel
