
#' xExecute
#'
#' Execute a (possibly side-effectual) function before
#' continuing with the previous method result.
#'
#' @section Type Signature:
#'     (any -> any) -> any -> any
#'
#' @param
#'      fn a unary function. The function to apply to the data in the kea object.
#'
#' @param
#'    val an arbitrary value. The contents of the kea object.
#'
#' @return
#'      The return value of the previous method.
#'
#' @section Corner Cases:
#'     None.
#'
#' @family methods
#'
#' @family inpure_functions
#'
#' @name xExecute

xExecute <- MakeFun('xExecute', function (fn, val) {

	MACRO( Must_Have_Arity(fn, 1) )

	MACRO( Try_Higher_Order_Function( fn(val) ) )
	val
})
