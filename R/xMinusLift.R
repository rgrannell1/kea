
#' xMinusLift
#' 
#' Compose two function with subtraction
#'
#' @param fn1 a function that returns a number.
#' @param fn2 a function that returns a number.
#'
#' @return a unary function of x.
#'
#' @template glossary
#'
#' @examples 
#' @export

xMinusLift <- function (fn1, fn2) {
	# (a -> number) -> (a -> number) -> (a -> number)
	# compose two functions with subtraction

	xPhoenix("-", fn1, fn2)
}
