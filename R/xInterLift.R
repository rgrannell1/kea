
#' xInterLift
#' 
#' Compose two function with the set intersection.
#'
#' @param fn1 a unary function
#' @param fn2 a unary function
#'
#' @return a unary function.
#'
#' @section Corner Cases: 
#'     returns the empty list if \code{coll} is length-zero.
#' @template glossary
#'
#'
#' @family function_lifting
#' @family higher_order_functions
#'
#' @example inst/examples/blank.R
#' @export

xInterLift <- function (fn1, fn2) {
	
	xPhoenix(xInter, fn1, fn2)
}
