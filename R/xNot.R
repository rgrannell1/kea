
#' xNot
#' 
#' Return the logical negation of a function.
#'
#' @param pred a predicate of any arity.
#'
#' @return a logical value.
#'
#' @template glossary
#'
#' @examples 
#' @export

#' @export

xNot <- function (pred) {
	# function -> function
	
	xCompose(function (x) !x, pred)
}
