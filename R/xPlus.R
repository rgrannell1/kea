
#' Compose two functions with addition.

#' @param fn_1 a function
#' @param fn_2 a function

#' @export

xPlus <- function (fn_1, fn_2) {
	# (a -> number) -> (a -> number) -> (a -> number)
	xPhoenix("+", fn_1, fn_2)
}
