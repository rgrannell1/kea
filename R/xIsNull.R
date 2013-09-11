
#' xIsNull
#' 
#' Is an element of a collection null?
#'
#' @param coll a collection
#'
#' @return a list.
#'
#' @section Corner Cases: 
#'     returns True if coll is Null.
#' @template glossary
#'
#' @examples 
#' @export

xIsNull <- function (coll) {

	pcall <- sys.call()
	require_a(traits$collection, coll, pcall)

	if (length(coll) == 0 && is.null(coll)) {
		# empty pairlist.
		True
	} else {
		ith <- 1
		res <- vector(mode = 'logical', length(coll))
		while (ith <= length(coll)) {
			res[ith] <- identical(coll[[ith]], Null)
			ith <- ith + 1
		}
		res		
	}
}

