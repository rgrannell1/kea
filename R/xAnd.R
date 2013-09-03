
#' xAnd
#' 
#' Return a function that tests if a pair of functions are true for its input.
#'
#' @param pred1 a predicate.
#' @param pred2 a predicate.
#'
#' @return a unary predicate.
#'
#' @template glossary.
#'
#' @examples 
#' @export

xAnd <- function (pred1, pred2) {
	# (a -> logical) -> (a -> logical) -> (a -> logical)
	
	xPhoenix("&&", pred1, pred2)
}
