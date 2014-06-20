
#' xExecute
#'
#' Execute a (possibly side-effectful) function before
#' continuing with the previous method result.
#'
#' @section Type Signature:
#'     (any -> any) -> any -> any
#'
#' @usage
#'      x_(  ) $ xExecute(fn)
#'
#' @param
#'      fn a unary function. The function to apply to the data in the kiwi object.
#'
#' @param
#'    val an arbitrary value. The contents of the kiwi object.
#'
#' @return
#'      The return value of the previous method.
#'
#' @section Corner Cases:
#'
#' @family methods
#'
#' @family inpure_functions
#'
#' @name xExecute

xExecute <- MakeFun('xExecute', function (fn, val) {

	fn(val)
	val
})
