
#' xIterate
#'
#' Iteratively apply a function to a value.
#'
#' @param
#'    fn a function.
#'
#' @param
#'    init an arbitrary value.
#'
#' @section Corner Cases:
#'    length-zero values of \code{init} are handled normally, since \code{init} is
#'    an arbitrary value. Potentially non-terminating.
#'
#' @return
#'    the result of successively applying \code{f} to \code{init}.
#'
#' @family higher_order_functions
#'
#' @rdname xIterate
#' @export

xIterate <- function (fn, init) {
	# (any -> any) -> any
	# iterate until higher-order returned.

	invoking_call <- sys.call()

	assert(
		!missing(fn), invoking_call,
		exclaim$parametre_missing(fn))

	assert(
		!missing(init), invoking_call,
		exclaim$parametre_missing(init))

	assert(
		!is.primitive(fn), invoking_call)

	assert(
		is_fn_matchable(fn), invoking_call,
		exclaim$must_be_matchable(
			fn, profile_object(fn)) )

	fn <- match.fun(fn)

	callCC(function (Return) {

		clone_env <- new.env(parent = environment(fn))
		clone_env$Return <- Return

		environment(fn) <- clone_env

		repeat {
			init <- try_higher_order(
				fn(init),
				invoking_call)
		}
	})
}
