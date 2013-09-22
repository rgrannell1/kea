
#' xPluck
#' 
#' Map over a collection of lists or pairlists,
#'     selecting fields in each element by name.
#'
#' @param str a string.
#' @param coll a list or pairlist of lists or pairlists.
#'
#' @return a list.
#'
#' @section Corner Cases: 
#'     returns the empty list if \code{coll} is length-zero. If str is length-zero
#'     then the empty string "" is used to match key-names.
#' @template glossary
#'
#' @examples 
#' @export

xPluck <- function (str, coll) {
	# Vector string -> Collection any -> Collection [any]

	pcall <- sys.call()

	assert(
		!missing(str), pcall)
	assert(
		!missing(coll), pcall)

	assert(
		is.character(str) && length(str) == 1, pcall)

	assert(
		is.list(coll) || is.pairlist(coll), pcall)

	assert(
		all( sapply(coll, is.recursive) ), pcall)

	if (length(coll) == 0) {
		list()
	} else {

		if (length(str) == 0) {
			str <- ""
		}

		lapply( coll, function (elem) {
			as.list( elem[[str]] )
		})
	}
}
