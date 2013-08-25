
#' Compose a binary function with functions as its left and right arguments.

#' @param fn_1 
#' @param fn_2 
#' @param fn_3 

#' @export

xBiCompose <- function (fn_1, fn_2, fn_3) {
	# the phoenix or Phi combinator

	pcall <- sys.call()
	pframe <- parent.frame()

	require_a("functionable", fn_1, pcall)
	require_a("functionable", fn_2, pcall)
	require_a("functionable", fn_3, pcall)

	fn_1 <- match.fun(fn_1)
	fn_2 <- match.fun(fn_2)
	fn_3 <- match.fun(fn_3)

	function (x) {
		fn_1( fn_2(x), fn_3(x) )
	}
}

#' @export

xPhoenix <- xBiCompose

#' @export

xSprime <- xPhoenix
