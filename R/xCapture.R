
#' xCapture
#'
#' Create a function that returns a particular value.
#'
#' @section Type Signature:
#'    any -> (...any -> any)
#'
#' @details
#'     \bold{xCapture} is the contant combinator - a function
#'     that takes an arguments and returns a function that
#'     always returns that value.
#'
#'     The function returned by \bold{xCapture} has ellipsis arguments,
#'     and ignores those arguments.
#'
#' @param
#'    val an arbitrary value. The value to close over with a
#'    closure.
#'
#' @return
#'    A variadic function that returns \bold{val}.
#'
#' @family function_modifying_functions
#'
#' @family basic_functions
#'
#' @example
#'    inst/examples/example-xCapture.R
#'
#' @rdname xCapture
#' @export

xCapture <- MakeFun(function (val) {

	MACRO( Must $ Not_Be_Missing(val) )

	function (...) {
		"a function created by xCapture."
		""
		val
	}
})

#' @rdname xCapture
#' @export

xK <- xCapture
