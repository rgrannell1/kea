
#' xNot
#' 
#' Return the logical negation of a function.
#'
#' @param pred a predicate of any arity.
#'
#' @return a predicate function of val.
#'
#' @template glossary
#'
#' @examples 
#' @export

#' @export

xNot <- function (pred) {
	# function -> function
	
	xCompose(function (val) !val, pred)
}
