
#' xIterate
#'
#' Iteratively apply a function to a value.
#'
#' @param
#'    fn a function.
#'
#' @param
#'    val an arbitrary value.
#'
#' @section Corner Cases:
#'    Length-zero values of \code{val} are treated as normal values,
#'    since \code{val} accepts an arbitrary value. Like while loops,
#'    \code{xIterate} is potentially non-terminating, and \code{Return( )}
#'    must be called to terminate the function.
#'
#' @return
#'    The result of successively applying \code{f} to \code{val}.
#'
#' @example
#'    inst/examples/example-xIterate.R
#'
#' @rdname xIterate
#' @export

xIterate <- function (fn, val) {
	# (any -> any) -> any
	# iterate until higher-order returned.

	invoking_call <- sys.call()

	assert(
		!missing(fn), invoking_call,
		exclaim$parametre_missing(fn))

	assert(
		!missing(val), invoking_call,
		exclaim$parametre_missing(val))

	assert_is_fn_matchable(fn, invoking_call)

	assert(
		!is.primitive(fn), invoking_call,
		exclaim$must_be_non_primitive(
			fn, summate(fn)))

	fn <- match_fn(fn)

	callCC(function (Return) {

		clone_env <- new.env(parent = environment(fn))
		clone_env$Return <- Return

		environment(fn) <- clone_env

		repeat {
			val <- try_higher_order(
				fn(val), invoking_call)
		}
	})
}
