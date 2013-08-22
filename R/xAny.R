
#' Are any elements in a collection true for a predicate?
#'
#' @param pred a unary function that returns a boolean value.
#' @param coll a list, vector or pairlist.
#'
#' @section Corner Cases:
#'     if \code{coll} is length zero then \code{FALSE} is returned.
#'
#' @export

#| function: xAny version: 0.1 finished: false 

xAny <- function (pred, coll) {
	# (any -> bool) -> Collection any -> bool
	# is a predicate true for some member of a collection?

	pcall <- sys.call()
	require_a("functionable", pred, pcall)
	require_a("collection", coll, pcall)

	pred <- match.fun(pred)
	require_a('unary function', pred)
	
	if (length(coll) == 0) {
		False
	} else {
		bools <- as.logical(lapply(coll, pred))

		if ( all(is.na(bools)) ) {
			False
		} else {
			any(bools)
		}
	}
}
