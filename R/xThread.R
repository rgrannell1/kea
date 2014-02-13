
#' xThread
#'
#' Iteratively apply a value to list of functions.
#'
#' @param
#'    val an arbitrary value. The value to feed to
#'    the input functions.
#'
#' @param
#'   fns a collection of unary functions. The functions
#'   to successively pipe the value through.
#'
#' @param
#'    ... see above.
#'
#' @return
#'    An arbitrary value.
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
#' @example
#'    inst/examples/example-xThread.R
#'
#' @rdname xThread
#' @export

xThread <- function (val, fns) {
	# any -> .... -> any
	# iteratively apply a value to each function in a list.

	invoking_call <- sys.call()

	insist $ must_not_be_missing(val)

	insist $ must_be_collection(fns, invoking_call)
	insist $ must_be_collection_of_fn_matchable(fns, invoking_call)

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
