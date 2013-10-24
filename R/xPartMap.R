
#' xPartMap
#' 
#' Partially apply xMap with a function. 
#'
#' @param fn a unary function.
#'
#' @return a unary function of val.
#'
#' @section Corner Cases: 
#'     returns the empty list if \code{coll} is length-zero.
#'
#' @template glossary
#'
#' @family higher_order_functions map_like_functions
#'
#' @example inst/examples/blank.R
#' @export

xPartMap <- function (fn) {
	function (val) xMap(fn, val)
}
