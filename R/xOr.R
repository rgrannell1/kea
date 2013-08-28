
#' xOr
#' 
#' Return a function that tests if either of two functions are true for a particular value.
#'
#' @param pred1 a predicate.
#' @param pred2 a predicate.
#'
#' @return a logical value.
#'
#' @template glossary
#'
#' @examples 
#' @export

xOr <- function (pred1, pred2) {
	# (a -> logical) -> (a -> logical) -> (a -> logical)
	xPhoenix("||", pred1, pred2)
}
