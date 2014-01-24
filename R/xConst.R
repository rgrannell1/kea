
#' xConst
#'
#' Create a function that returns a particular value.
#'
#' @details
#' \bold{xConst}.
#'
#'
#'
#'
#'
#' @param
#'    val an arbitrary value.
#'
#' @return
#'    A variadic function.
#'
#' @family function_modifying_functions
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
