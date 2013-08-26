
#' Compose two functions with addition.

#' @param fn1 a function
#' @param fn2 a function

#' @export

xPlus <- function (fn1, fn2) {
	# (a -> number) -> (a -> number) -> (a -> number)
	xPhoenix("+", fn1, fn2)
}
