
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
#'    length-zero values of \code{val} are handled normally, since \code{val} is
#'    an arbitrary value. Potentially non-terminating.
#'
#' @return
#'    the result of successively applying \code{f} to \code{val}.
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

	assert(
		is_fn_matchable(fn), invoking_call,
		exclaim$must_be_matchable(
			fn, summate(fn)) )

	assert(
		!is.primitive(fn), invoking_call,
		exclaim$must_be_non_primitive(
			fn, summate(fn)))

	fn <- match.fun(fn)

	callCC(function (Return) {

		clone_env <- new.env(parent = environment(fn))
		clone_env$Return <- Return

		environment(fn) <- clone_env

		repeat {
			val <- try_higher_order(
				fn(val),
				invoking_call)
		}
	})
}
