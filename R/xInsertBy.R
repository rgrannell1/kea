
#' xInsertBy
#' 
#' Insert an element before the first element in a collection that it is smaller than.
#'
#' @param pred a binary predicate.
#' @param val an arbitrary value.
#' @param coll a collection
#'
#' @return a list.
#'
#' @section Corner Cases: 
#'     returns the empty list if \code{coll is length-zero}.
#' @template glossary
#'
#' @examples 
#' @export

xInsertBy <- function (pred, val, coll) {
	# (any -> any -> logical) -> any -> Collection any -> [any]

	pcall <- sys.call()
	require_a('functionable', pred, pcall)
	require_a('arbitrary', val, pcall)
	require_a('collection', coll, pcall)

	pred <- match.fun(pred)
	coll <- as.list(coll)

	if (length(coll) == 0) {
		list(val)
	} else {

		ith <- 1
		while (ith <= length(coll)) {

			val_is_greater <- pred( val, coll[[ith]] )
			
			if (val_is_greater) {
				return( c(
					head(coll, ith - 1), 
					list(val), 
					tail(coll, -(ith - 1)) ))

			} else {
				ith <- ith + 1
			}
		}
		c(coll, val)
	}
}
