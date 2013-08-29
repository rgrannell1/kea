
#' xFmap
#' 
#' Partially apply xMap with a function. 
#'
#' @param fn a unary function.
#' @param coll a collection
#'
#' @return a unary function of x.
#'
#' @section Corner Cases: 
#'     returns the empty list if \code{coll is length-zero}.
#' @template glossary
#'
#' @examples 
#' @export

xFmap <- function (fn) {
	function (x) xMap(fn, x)
}
