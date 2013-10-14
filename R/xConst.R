
#' xConst
#' 
#' Create a function that returns a particular value.
#'
#' @param val an arbitrary value.
#'
#' @return a variadic function.
#'
#'
#' @template glossary
#'
#' @family higher_order_function
#'
#' @example inst/examples/blank.R
#' @export

xConst <- function (val) {
	# return a function that closes over the variable val.

	assert(
		!missing(val), sys.call(),
		exclaim$parameter_missing(val))

	function (...) {
		val
	}
}

#' @export

xKestrel <- xConst

#' @export

xK <- xKestrel
