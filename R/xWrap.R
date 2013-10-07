
#' xWrap
#' 
#' Wrap a function within another function
#'
#' @param fn1 an arbitrary function.
#' @param fn2 a binary function, that takes \code{fn1} as its first argument and 
#'     additional arguments as ith second argument.
#'
#' @return a variadic function.
#'
#' @section Corner Cases: 
#' @template glossary
#'
#' @examples inst/examples/blank.R
#' @export

# fn2 takes func to wrap, extra args.

xWrap <- function (fn1, fn2) {
	# wrap a function in a second function, 
	# still allowing arguments to be passed in.

	pcall <- sys.call()

	assert(
		!missing(fn1), pcall,
		exclaim$parameter_missing(fn1))
	assert(
		!missing(fn2), pcall,
		exclaim$parameter_missing(fn2))

	assert(
		is.function(fn1) || is.symbol(fn1) || 
		(is.character(fn1) && length(fn1) == 1), pcall,
		exclaim$must_be_matchable(fn1))

	assert(
		is.function(fn2) || is.symbol(fn2) || 
		(is.character(fn2) && length(fn2) == 1), pcall,
		exclaim$must_be_matchable(fn2))

	fn1 <- match.fun(fn1)
	fn2 <- match.fun(fn2)

	assert(
		xArity(fn2) %in% c(2, Inf), pcall)

	function (...) {
		fn2(fn1, ...)
	}
}

# Wrap is definitely A T-combinator, 
# if not necessarily the canonical one.
#' @export

xThrush <- xWrap

#' @export

xT <- xWrap
