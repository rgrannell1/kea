
#' xThread
#'
#' Iteratively apply a value to list of functions.
#'
#' @param
#'    val an arbitrary value
#'
#' @param
#'    fns several unary functions.
#'
#' @param
#'    ... see above.
#'
#' @return
#'    A list.
#'
#' @section Corner Cases:
#'    returns the empty list if \code{coll} is length-zero.
#'
#' @family function_modifying_functions
#'
#' @family function_application_functions
#'
#' @template
#'    Variadic
#'
#' @rdname xThread
#' @export

xThread <- function (val, fns) {
	# any -> .... -> any
	# iteratively apply a value to each function in a list.

	invoking_call <- sys.call()

	assert(
		!missing(val), invoking_call,
		exclaim$parametre_missing(val))

	assert(
		all( vapply(fns, is_fn_matchable, logical(1)) ), invoking_call,
		exclaim$must_be_recursive_of_matchable(
			fns, summate(fns)) )

	for (ith in seq_along(fns)) {

		val <- try_higher_order(
			fns[[ith]]( val ), invoking_call)
	}
	val
}

#' @rdname xThread
#' @export

xThread... <- function (val, ...) {
	xThread(val, list(...))
}
