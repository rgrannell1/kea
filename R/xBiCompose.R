
#' xBiCompose
#' 
#' Compose a binary function with two other functions.
#'
#' @param fn1 a binary function.
#' @param fn2 a unary function.
#' @param fn3 a unary function.
#'
#' @return returns a unary function of x.
#'
#'
#' @template glossary
#'
#' @examples 
#' @export

#' @export

xBiCompose <- function (fn1, fn2, fn3) {
	# the phoenix or Phi combinator

	pcall <- sys.call()
	pframe <- parent.frame()

	require_a("functionable", fn1, pcall)
	require_a("functionable", fn2, pcall)
	require_a("functionable", fn3, pcall)

	fn1 <- match.fun(fn1)
	fn2 <- match.fun(fn2)
	fn3 <- match.fun(fn3)

	function (...) {
		fn1( fn2(...), fn3(...) )
	}
}

#' @export

xPhoenix <- xBiCompose

#' @export

xS. <- xPhoenix
