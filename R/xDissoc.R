
#' xDissoc
#' 
#' Split a named list into a list of name: value lists.
#'
#' @param coll a list or pairlist of list or pairlist pairs, with the first element being a 
#'	string and the second element being any value.
#'
#' @return a named list.
#'
#' @section Corner Cases: 
#'     returns \code{list()} if \code{coll} is length-zero.
#' @template glossary
#'
#' @example inst/examples/blank.R
#' @export

xDissoc <- function (coll) {
	# Named Collection any -> [[string, any]]

	pcall <- sys.call()

	assert(
		!missing(coll), pcall, 
		exclaim$parameter_missing(coll))

	coll <- dearrowise(coll)

	assert(
		length(names(coll)) == length(coll), pcall)

	if (length(coll) == 0) {
		list()
	} else {
		xZip( names(coll), unname(coll) )
	}
}
