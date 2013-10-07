
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
#' @examples inst/examples/blank.R
#' @export

xPluck <- function (str, coll) {
	# Vector string -> Collection any -> Collection [any]

	pcall <- sys.call()

	assert(
		!missing(str), pcall,
		exclaim$parameter_missing(str))
	assert(
		!missing(coll), pcall, 
		exclaim$parameter_missing(coll))

	assert(
		is.character(str) && length(str) == 1, pcall)

	assert(
		is.recursive(coll), pcall)

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
