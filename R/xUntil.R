
#' Iteratively apply a function until a predicate is met.
#'
#' @param pred a predicate function.
#'
#' @param fn a function.
#'
#' @param init an arbitrary value.
#'
#' @section Corner Cases:
#'	 length-zero values of \code{init} are handled normally, since \code{init} is 
#'	 an arbitrary value. Potentially non-terminating.
#'
#' @return the result of successively applying \code{f} to \code{init}.
#'
#' @family higher_order_function
#'
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

	pred <- dearrowise(pred)
	fn <- dearrowise(fn)
	init <- dearrowise(init)

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
