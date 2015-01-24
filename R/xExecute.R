
#' xExecute
#'
#' Execute a (possibly side-effectual) function before
#' continuing with the previous method result.
#'
#' @details
#'     \bold{xExecute} is mainly intended to be used as a method; it allows
#'     a side-effectful function to be run between method calls, keeping the
#'     previous result.
#'
#'     \code{x_(1:10) $ xExecute(function () print('doing something')) $ x_SumBy(xI)}
#'
#'     The above call wraps 1...10 in a kea object, prints 'doing something', and proceeds
#'     to sum the numbers 1...10.
#'
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

xExecute <- MakeFun(function (fn, val) {

	MACRO( Must_Have_Arity(fn, 1) )

	MACRO( Try_Higher_Order_Function( fn(val) ) )
	val
})
