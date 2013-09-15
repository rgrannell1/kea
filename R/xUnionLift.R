
#' xUnionLift
#' 
#' Compose two function with the set union.
#'
#' @param fn1 a unary function that returns a collection.
#' @param fn2 a unary function that returns a collection.
#'
#' @return a unary function.
#'
#' @section Corner Cases: 
#'     returns the empty list if \code{coll} is length-zero.
#' @template glossary
#'
#' @examples 
#' @export


xUnionLift <- function (fn1, fn2) {

	xPhoenix(xUnion, fn1, fn2)
}
