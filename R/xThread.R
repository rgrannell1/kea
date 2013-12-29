
#' xThread
#'
#' Iteratively apply a value to list of functions.
#'
#' @param
#'    init an arbitrary value
#'
#' @param
#'    fns several unary functions.
#'
#' @return
#'    a list.
#'
#' @section Corner Cases: q
#'    returns the empty list if \code{coll} is length-zero.
#'
#' @family function_modifying_functions
#'
#' @family variadic_functions
#'
#' @rdname xThread
#' @export

xThread <- function (init, fns) {
	# any -> .... -> any
	# iteratively apply a value to each function in a list.

	invoking_call <- sys.call()

	assert(
		!missing(init), invoking_call,
		exclaim$parametre_missing(init))

	assert(
		all(sapply(fns, is_fn_matchable)), invoking_call,
		exclaim$must_be_recursive_of_matchable(fns))

	for (ith in seq_along(fns)) {

		init <- try_higher_order(
			fns[[ith]]( init ), invoking_call)
	}
	init
}

#' @rdname xThread
#' @export

xThread... <- function (init, ...) {
	xThread(init, list(...))
}
