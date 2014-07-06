
#' xThread
#'
#' Iteratively apply a list of functions to a value.
#'
#' @section Type Signature:
#'     any -> |(any -> any)| -> any
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
#'    If no functions are supplied by \bold{fns}, \bold{val} is returned as is.
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

xThread <- MakeFun('xThread', function (val, fns) {

	# -- iteratively pipe the starting value
	# -- through each function.
	for (ith in seq_along(fns)) {
		val <- MACRO( Try_Higher_Order_Function( fns[[ith]]( val ) ) )
	}

	val
})

#' @rdname xThread
#' @export

xThread_ <- MakeVariadic(xThread, 'fns')
