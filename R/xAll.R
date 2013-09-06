
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
	require_a("functionable", pred, pcall)
	require_a("collection", coll, pcall)

	pred <- match.fun(pred)
	require_a('unary function', pred)

	if (length(coll) == 0) {
		logical(0)
	} else {
		is_match <- vapply(coll, pred, logical(1))

		if ( any(`_`(is_match)) ) {
			False
		} else {
			all(is_match)
		}
	}
}
