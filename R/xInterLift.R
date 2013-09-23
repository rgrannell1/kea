
#' xInterLift
#' 
#' Compose two function with the set intersection.
#'
#' @param fn1 a unary function.
#'
#' @return a unary function.
#'
#' @section Corner Cases: 
#'     returns the empty list if \code{coll} is length-zero.
#' @template glossary
#'
#' @examples inst/examples/blank.R
#' @examples inst/examples/blank.R
#' @examples inst/examples/blank.R
#' @examples inst/examples/blank.R 
#' @export

xInterLift <- function (fn1, fn2) {
	
	xPhoenix(xInterLift, fn1, fn2)
}
