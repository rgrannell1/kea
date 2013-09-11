
#' xAnd
#' 
#' Return a function that then tests if a pair of functions are true for its input.
#'
#' @param pred1 an n-ary predicate.
#' @param pred2 an n-ary predicate.
#'
#' @return a n-ary predicate.
#'
#' @template glossary.
#'
#' @examples 
#' @export

xAnd <- function (pred1, pred2) {
	# (a -> logical) -> (a -> logical) -> (a -> logical)
	# Return a function that then tests if a
	# pair of functions are true for its input.
	
	xPhoenix("&&", pred1, pred2)
}
