
#' xAsUnary
#' 
#' Create a function that takes one argument, and applies each element of
#' that argument as a parameter of its underlying function.
#'
#' @param  fn an arbitrary function.
#'
#' @return a unary function of x.
#'
#'
#' @template glossary
#'
#' @examples inst/examples/blank.R
#' @examples inst/examples/blank.R
#' @examples inst/examples/blank.R
#' @examples inst/examples/blank.R 
#' @export

xAsUnary <- function (fn) {
	# (... -> b) -> (a -> b)
	# dual to xAsVariadic. 
	# takes a function that takes a many values and 
	# makes it into a function that takes one list.

	pcall <- sys.call()
	
	assert(
		!missing(fn), pcall)

	assert(
		is.function(fn) || is.symbol(fn) || 
		(is.character(fn) && length(fn) == 1), pcall)

	fn <- match.fun(fn)
	remove(pcall)

	function (x) {
		xApply(fn, as.list(x))
	}
}
