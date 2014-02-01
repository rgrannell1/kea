
#' xConst
#'
#' Create a function that returns a particular value.
#'
#' @details
#' \bold{xConst} is the contant combinator - a function
#' that takes an arguments and returns a function that
#' always returns that value.
#'
#' The function returned by \bold{xConst} has ellipsis arguments,
#' and ignores those arguments.
#'
#' @param
#'    val an arbitrary value. The value to close over with a
#'    closure.
#'
#' @return
#'    A variadic function that returns \bold{val}.
#'
#' @family function_modifying_functions
#'
#'
#' @example
#'    inst/examples/example-xConst.R
#'
#' @rdname xConst
#' @export

xConst <- function (val) {
	# any -> (... -> any)
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

#' @rdname xConst
#' @export

xK <- xConst
