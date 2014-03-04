
#' xThread
#'
#' Iteratively apply a list of functions to a value.
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

xThread <- MakeFun(function (val, fns) {
	# any -> .... -> any
	# iteratively apply a value to each function in a list.

	invoking_call <- sys.call()

	MACRO( arrow ::: Must $ Not_Be_Missing(val) )
	MACRO( arrow ::: Must $ Not_Be_Missing(fns) )

	MACRO( arrow ::: Must $ Be_Collection(fns) )
	MACRO( arrow ::: Must $ Be_Collection_Of_Fn_Matchable(fns) )

	for (ith in seq_along(fns)) {

		val <- try_hof(
			fns[[ith]]( val ), invoking_call)
	}
	val
})

#' @rdname xThread
#' @export

xThread... <- function (val, ...) {
	xThread(val, list(...))
}
