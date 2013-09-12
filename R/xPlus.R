
#' xPlus
#' 
#' Compose two functions with addition.
#'
#' @param fn1 a unary function that returns a number.
#' @param fn2 a unary function that returns a number.
#'
#' @template glossary
#'
#' @return a number.
#'
#' @examples 
#' @export

#' @export

xPlus <- function (fn1, fn2) {
	# (a -> number) -> (a -> number) -> (a -> number)
	
	xPhoenix("+", fn1, fn2)
}
