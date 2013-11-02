
#' Iteratively apply a function to a value.
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
#' @family higher_order_functions
#'
#' @export

xIterate <- function (fn, init) {
	# (any -> any) -> any
	# iterate until higher-order returned.

	pcall <- sys.call()

	assert(
		!missing(fn), pcall,
		exclaim$parameter_missing(fn))

	assert(
		!missing(init), pcall,
		exclaim$parameter_missing(init))

	fn <- dearrowise(fn)
	init <- dearrowise(init)

	assert(
		is_fn_matchable(fn), pcall,
		exclaim$must_be_matchable(fn))

	fn <- match.fun(fn)

	callCC(function (Return) {

		clone_env <- new.env(parent = environment(fn))
		clone_env$Return <- Return

		environment(fn) <- clone_env

		repeat {
			init <- fn(init)
		}
	})
}
