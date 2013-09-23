
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
#' @examples inst/examples/blank.R
#' @export

#' @export

xNot <- function (pred) {
	# function -> function
	
	assert(
		!missing(pred), sys.call())

	xCompose(function (val) !val, pred)
}
