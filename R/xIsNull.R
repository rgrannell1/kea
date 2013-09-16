
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
	assert(
		is.vector(coll) || is.pairlist(coll), pcall)

	if (length(coll) == 0 && is.null(coll)) {
		# empty pairlist.
		True
	} else {
		res <- vector(mode = 'logical', length(coll))

		for (ith in seq_along(coll)) {
			res[ith] <- identical(coll[[ith]], Null)
		}
		res		
	}
}

