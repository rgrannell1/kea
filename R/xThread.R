
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
#'    Returns the empty list if \bold{coll} is length-zero.
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

	insist$must_be_collection(fns, invoking_call)
	insist$must_be_collection_of_fn_matchable(fns, invoking_call)

	for (ith in seq_along(fns)) {

		val <- try_hof(
			fns[[ith]]( val ), invoking_call)
	}
	val
}

#' @rdname xThread
#' @export

xThread... <- function (val, ...) {
	xThread(val, list(...))
}
