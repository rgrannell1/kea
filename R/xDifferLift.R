
#' xDifferLift
#' 
#' Compose two functions with the set difference.
#'
#' @param fn1 a unary function
#'
#' @return fn1 a unary function.
#'
#' @section Corner Cases: 
#'     returns the empty list if \code{coll} is length-zero.
#' @template glossary
#'
#' @examples 
#' @export

xDifferLift <- function (fn1, fn2) {

	xPhoenix(xDiffer, fn1, fn2)
}