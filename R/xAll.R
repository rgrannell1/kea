
#' Are all elements in a collection true for a predicate?
#'
#' @param pred a unary function that returns a boolean value.
#' @param coll a list, vector or pairlist.
#'
#' @section Corner Cases:
#'     if \code{coll} is length zero then \code{True} is returned. If any values
#'     are na then false is automatically returned.
#'
#' @export

#| function: xAll version: 0.1 finished: false 

xAll <- xAutoPartial(function (pred, coll) {
	# (any -> logical) -> Collection any -> bool
	# is a predicate true for every member of a collection?

	pcall <- sys.call()
	require_a("functionable", pred, pcall)
	require_a("collection", coll, pcall)

	pred <- match.fun(pred)
	require_a('unary function', pred)

	if (length(coll) == 0) {
		True
	} else {
		is_match <- vapply(coll, pred, True)

		if ( any(is.na(is_match)) ) {
			False
		} else {
			all(is_match)
		}
	}
})
