
#' xEqual
#' 
#' Return a function that tests if a pair of functions are equal for its input.
#'
#' @param fn1 an arbitrary function
#' @param fn2 an arbitrary function.
#'
#' @return an n-ary predicate.
#'
#' @section Corner Cases: 
#'     returns the empty list if \code{coll is length-zero}.
#' @template glossary
#'
#' @examples 
#' @export


xEqual <- function (fn1, fn2) {
	# function -> function -> function

	xPhoenix('==', fn1, fn2)
}
