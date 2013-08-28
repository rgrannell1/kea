
#' xModulo
#'
#' Compose two function with the modulo operator.
#' 
#' @param fn1 a unary function that returns a number.
#' @param fn2 a unary function that returns a number.
#'
#' @return a number.
#'
#' @template glossary
#'
#' @examples 
#' @export

xMod <- function (fn1, fn2) {
	# (a -> logical) -> (a -> logical) -> (a -> logical)
	xPhoenix("%%", fn1, fn2)
}
