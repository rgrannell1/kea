
#' Iteratively apply a function until a predicate is met.
#'
#' @param predicate a unary function that returns a logical value, or a 
#'     symbol or name identifying such a function.
#' @param unary a unary function function, or a
#'     symbol or name identifying such a function.
#' @param initial an arbitrary value.
#'
#' @section Corner Cases:
#'     length-zero values of \code{initial} are handled normally, since \code{initial} is 
#'     an arbitrary value. Potentially non-terminating.
#'
#' @return the result of successively applying \code{f} to \code{initial}.
#' @family arrow-maps
#' @export

#| function: xUntil version: 0.1 finished: false 

xUntil <- function (pred, fn, init) {
	# (any -> boolean) -> (any -> any) -> any
	# repeatedly apply function to initial, until predicate of 
	# the result is true.

	pcall <- sys.call()
	require_a("functionable", pred, pcall)
	require_a("functionable", fn, pcall)
	require_a('arbitrary', init, pcall)

	pred <- match.fun(pred)
	fn <- match.fun(fn)

	require_a('unary function', pred, pcall)
	require_a('unary function', fn, pcall)

	repeat {
		if (pred(init)) break
		init <- fn(init)
	}
	init
}
