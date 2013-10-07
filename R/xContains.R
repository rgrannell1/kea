
#' xContains
#' 
#' Check if a collection contains a value.
#'
#' @param coll a collection
#'
#' @return a list.
#'
#' @section Corner Cases: 
#'     various types of NA are not-distinguished between. Type conversion is not carried out.
#' @template glossary
#'
#' @examples inst/examples/blank.R
#' @export

xContains <- function (coll, val) {
	# Collection any -> any -> Vector logical

	pcall <- sys.call()

	assert(
		!missing(coll), pcall, 
		exclaim$parameter_missing(coll))
	assert(
		!missing(val), pcall,
		exclaim$parameter_missing(val))

	assert(
		is_collection(coll), pcall,
		exclaim$must_be_collection(coll))

	if (length(coll) == 0) {
		logical(0)
	} else {
		is_match <- vapply(
			coll, 
			function (elem) {
				identical(elem, val, single.NA = True)
			}, 
			logical(1), 
			USE.NAMES = False)

		if ( all(is.na(is_match)) ) {
			False
		} else {
			any(is_match)
		}
	}
}
