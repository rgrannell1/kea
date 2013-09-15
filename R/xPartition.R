
#' xPartition
#' 
#' Divide elements in a collection into lists based on an equivelence predicate.
#'
#' @param pred a binary predicate function.
#' @param coll a collection
#'
#' @return a list of lists.
#'
#' @section Corner Cases: 
#'     returns the empty list if \code{coll} is length-zero.
#' @template glossary
#'
#' @examples 
#' @export

xPartition <- function (pred, coll) {
	# partition a set into an equivalence class.

	pcall <- sys.call()

	assert(
		is.function(pred) || is.symbol(pred) || 
		(is.character(pred) && length(pred) == 1), pcall)
	
	pred <- match.fun(pred)

	assert(
		xArity(pred) %in% c(2, Inf), pcall)

	assert(
		is.vector(coll) || is.pairlist(coll), pcall)

	if (length(coll) == 0) {
		list()
	} else if (length(coll) == 1) {
		list( coll[[1]] )
	} else {

		parts <- list()

		parts[[1]] <- list( coll[[1]] )
		colls <- coll[ 2:length(coll) ]

		ith <- 2
		while (ith <= length(coll)) {

			elem <- coll[[ith]]

			jth <- 1
			is_match <- False
			while (jth <= length(parts) && !is_match) {

				if (pred( elem, parts[[jth]][[1]] )) {
					parts[[jth]] <- c( parts[[jth]], elem )
					is_match <- True
				}
				jth <- jth + 1
			}
			if (!is_match) {
				parts <- c(parts, list(elem))
			}

			ith <- ith + 1
		}
		parts
	}
}
