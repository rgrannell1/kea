
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
#'
#' @family higher_order_functions
#'
#' @example inst/examples/blank.R
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

	str <- dearrowise(str)
	coll <- dearrowise(coll)

	assert(
		is.character(str) && length(str) == 1, pcall,
		exclaim$must_be_string(str))

	assert(
		is.recursive(coll), pcall,
		exclaim$must_be_recursive(coll))

	assert(
		all( sapply(coll, is.recursive) ), pcall,
		exclaim$must_be_recursive_of_collections(coll))

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
