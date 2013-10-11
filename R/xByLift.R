
#' xByLift
#' 
#' Compose two functions with multiplication.
#'
#' @param fn1 a unary function that returns a number.
#' @param fn2 a unary function that returns a number.
#'
#' @template glossary
#'
#' @return a unary function.
#'
#' @family function_lifting
#' @family higher_order_function
#'
#' @examples inst/examples/blank.R
#' @export

xByLift <- function (fn1, fn2) {
	# (a -> number) -> (a -> number) -> (a -> number)
	
	xPhoenix("*", fn1, fn2)
}
