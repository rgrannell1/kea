
#' xPartMap
#' 
#' Partially apply xMap with a function. 
#'
#' @param fn a unary function.
#' @param coll a collection
#'
#' @return a unary function of val.
#'
#' @section Corner Cases: 
#'     returns the empty list if \code{coll} is length-zero.
#' @template glossary
#'
#' @examples 
#' @export

xPartMap <- function (fn) {
	function (val) xMap(fn, val)
}
