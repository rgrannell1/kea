
#' Iteratively apply a function while a predicate is met.
#'
#' @param pred a unary function that returns a logical value, or a 
#'	 symbol or name identifying such a function.
#' @param fn an arbitrary function, or a
#'	 symbol or name identifying such a function.
#' @param init an arbitrary value.
#'
#' @section Corner Cases:
#'	 length-zero values of \code{x} are handled normally, since \code{x} is 
#'	 an arbitrary value. Potentially non-terminating.
#'
#' @return the result of successively applying \code{fn} to \code{x}.
#'
#' @family higher_order_function
#'
#' @export

xWhile <- function (pred, fn, init) {
		# (any -> boolean) -> (any -> any) -> any
		# repeatedly apply unary to x, until p of 
		# the result is true"

		pcall <- sys.call()

		assert(
			!missing(pred), pcall)
		assert(
			!missing(fn), pcall)
		assert(
			!missing(init), pcall)

		assert(
			is.function(pred) || is.symbol(pred) || 
			(is.character(pred) && length(pred) == 1), pcall)
		assert(
			is.function(fn) || is.symbol(fn) || 
			(is.character(fn) && length(fn) == 1), pcall)

		fn <- match.fun(fn)
		pred <- match.fun(pred)

		repeat {
			is_match <- pred(init)

			assert(is.logical(is_match), pcall)

			if (!is_match) break
			init <- fn(init)
		}
		init
}
