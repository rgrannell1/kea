
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

	assert(
		is.function(fn1) || is.symbol(fn1) || 
		(is.character(fn1) && length(fn1) == 1), pcall)
	
	assert(
		is.function(fn2) || is.symbol(fn2) || 
		(is.character(fn2) && length(fn2) == 1), pcall)

	assert(
		is.function(fn3) || is.symbol(fn3) || 
		(is.character(fn3) && length(fn3) == 1), pcall)

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
