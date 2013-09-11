
#' Iteratively apply a function while a predicate is met.
#'
#' @param pred a unary function that returns a logical value, or a 
#'	 symbol or name identifying such a function.
#' @param fn an arbitrary function, or a
#'	 symbol or name identifying such a function.
#' @param x an arbitrary value.
#'
#' @section Corner Cases:
#'	 length-zero values of \code{x} are handled normally, since \code{x} is 
#'	 an arbitrary value. Potentially non-terminating.
#'
#' @return the result of successively applying \code{fn} to \code{x}.
#' @family arrow-maps
#' @export

xWhile <- function (pred, fn, init) {
		# (any -> boolean) -> (any -> any) -> any
		# repeatedly apply unary to x, until p of 
		# the result is true"

		pcall <- sys.call()
		require_a(traits$functionable, pred, pcall)
		require_a(traits$functionable, fn, pcall)
		require_a("arbitrary", init, pcall)

		fn <- match.fun(fn)
		pred <- match.fun(pred)

		require_a('unary function', pred, pcall)
		require_a('unary function', fn, pcall)

		repeat {
			is_match <- pred(init)

			stopifnot(is.logical(is_match))

			if (!is_match) break
			init <- fn(init)
		}
		init
}
