
#' xAll
#' 
#' Are all elements in a collection true for a predicate?
#'
#' @param pred a predicate.
#' @param coll a collection.
#'
#' @section Corner Cases:
#'	 if \code{coll} is length zero then True is returned.
#'   If any values are na then \code{False} is returned. 
#'
#' @template glossary
#'
#' @return a boolean value.
#'
#' @examples 
#' @export

xAll <- function (pred, coll) {
	# (any -> logical) -> Collection any -> bool
	# is a predicate true for every member of a collection?

	pcall <- sys.call()

	assert(
		is.function(pred) || is.symbol(pred) || 
		(is.character(pred) && length(pred) == 1), pcall)
	
	assert(
		is.vector(coll) || is.pairlist(coll), pcall)

	pred <- match.fun(pred)
	
	assert(
		xArity(pred) %in% c(1, Inf), pcall)

	if (length(coll) == 0) {
		logical(0)
	} else {
		is_match <- vapply(coll, pred, logical(1), USE.NAMES = False)

		if ( any(is.na(is_match)) ) {
			False
		} else {
			all(is_match)
		}
	}
}
