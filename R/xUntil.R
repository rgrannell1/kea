
#' Iteratively apply a function until a predicate is met.
#'
#' @param predicate a unary function that returns a logical value, or a 
#'	 symbol or name identifying such a function.
#' @param unary a unary function function, or a
#'	 symbol or name identifying such a function.
#' @param init an arbitrary value.
#'
#' @section Corner Cases:
#'	 length-zero values of \code{init} are handled normally, since \code{init} is 
#'	 an arbitrary value. Potentially non-terminating.
#'
#' @return the result of successively applying \code{f} to \code{init}.
#' @family arrow-maps
#' @export

xUntil <- function (pred, fn, init) {
	# (any -> boolean) -> (any -> any) -> any
	# repeatedly apply function to init, until predicate of 
	# the result is true.

	pcall <- sys.call()

	assert(
		!missing(pred), pcall,
		exclaim$parameter_missing(pred))
	assert(
		!missing(fn), pcall, 
		exclaim$parameter_missing(fn))
	assert(
		!missing(init), pcall, 
		exclaim$parameter_missing(init))

	assert(
		is_fn_matchable(pred), pcall,
		exclaim$must_be_matchable(pred))

	assert(
		is_fn_matchable(fn), pcall, 
		exclaim$must_be_matchable(fn))

	pred <- match.fun(pred)
	fn <- match.fun(fn)

	repeat {
		is_match <- pred(init)

		assert(is.logical(is_match), pcall)

		if (is_match) break
		init <- fn(init)
	}

	init
}
