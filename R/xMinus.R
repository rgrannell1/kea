
#' xMinus
#' 
#' Compose two function with multiplication/
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

xMinus <- function (fn1, fn2) {
	# (a -> number) -> (a -> number) -> (a -> number)
	xPhoenix("-", fn1, fn2)
}
