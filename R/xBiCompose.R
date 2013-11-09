
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
#'
#'
#' @example inst/examples/blank.R
#' @export

#' @export

xBiCompose <- function (fn1, fn2, fn3) {
	# the phoenix or Phi combinator

	parent_call <- sys.call()
	parent_frame <- parent.frame()

	assert(
		!missing(fn1), parent_call,
		exclaim$parameter_missing(fn1))

	assert(
		!missing(fn2), parent_call,
		exclaim$parameter_missing(fn2))

	assert(
		!missing(fn3), parent_call,
		exclaim$parameter_missing(fn3))

	fn1 <- dearrowise(fn1)
	fn2 <- dearrowise(fn2)
	fn3 <- dearrowise(fn3)

	assert(
		is_fn_matchable(fn1), parent_call,
		exclaim$must_be_matchable(fn1))

	assert(
		is_fn_matchable(fn2), parent_call,
		exclaim$must_be_matchable(fn2))

	assert(
		is_fn_matchable(fn3), parent_call,
		exclaim$must_be_matchable(fn3))

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
