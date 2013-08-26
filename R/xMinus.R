
#' Compose two functions with subtraction.

#' @param fn1 a function
#' @param fn2 a function

#' @export

xMinus <- function (fn1, fn2) {
	# (a -> number) -> (a -> number) -> (a -> number)
	xPhoenix("-", fn1, fn2)
}
