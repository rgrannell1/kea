
#' xSegment
#' 
#' Divide a collection into segments of fixed length.
#'
#' @param num a nonnegative whole number.
#' @param coll a collection
#'
#' @return a list of n-element lsits.
#'
#' @section Corner Cases:
#'	 the final list in the return value will have less than \code{num}
#'	 elements if \code{length(coll)} is not evenly divisible by \code{num}.
#'	 if \code{coll} is length-zero, the empty list is returned.
#'
#' @template glossary
#'
#' @examples 
#' @export

xSegment <- function (num, coll) {
	# integer -> Collection any -> [[any]]
	# groups coll into chunks of num,
	# when possible.

	pcall <- sys.call()	

	require_a('positive whole', num, pcall)
	assert(
		is.vector(coll) || is.pairlist(coll), pcall)

	if (length(coll) == 0) {
		list()
	} else {
		lapply(
			seq(1, to = length(coll), by = num),
			function (lower) {
				as.list(coll[ lower:min(length(coll), lower + num - 1) ])
		})
	}
}
