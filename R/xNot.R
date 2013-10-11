
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
#' @family higher_order_function
#'
#' @examples inst/examples/blank.R
#' @export

xNot <- function (pred) {
	# function -> function
	
	assert(
		!missing(pred), sys.call(),
		exclaim$parameter_missing(pred))

	xCompose(function (val) !val, pred)
}
