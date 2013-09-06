
#' xTakeWhile
#' 
#' Take every element in a collection from the start until a predicate returns false.
#'
#' @param pred a predicate.
#' @param coll a collection
#'
#' @return a list.
#'
#' @section Corner Cases: 
#'     returns the empty list if \code{coll is length-zero} or the first element of 
#'     \code{coll} returns false for the predicate. Na values are considered false.
#' @template glossary
#'
#' @examples 
#' @export

xTakeWhile <- function (pred, coll) {
	# (any -> boolean) -> Collection any -> [any]
	# take every element until pred returns false

	pcall <- sys.call()
	require_a("functionable", pred, pcall)
	require_a("collection", coll, pcall)

	pred <- match.fun(pred)
	require_a("unary function", pred)

	if (length(coll) == 0) {
		list()
	} else {
		ith <- 1
		while (ith <= length(coll)) {

			is_match <- pred( coll[[ith]] )
			stopifnot(is.logical(is_match))

			if (!isTRUE(is_match)) {
				return ( as.list(head(coll, ith - 1)) )
			}
			ith <- ith + 1
		}
		coll		
	}
}
