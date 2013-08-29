
#' xDissoc
#' 
#' Convert a list of name, value lists into a named list.
#'
#' @param coll a list or pairlist of list or pairlist pairs, with the first element being a 
#'	string and the second element being any value.
#'
#' @return a named list.
#'
#' @section Corner Cases: 
#'     returns \code{list()} if \code{coll is length-zero}.
#' @template glossary
#'
#' @examples 
#' @export

xDissoc <- function (coll) {
	# Named Collection any -> [[string, any]]

	pcall <- sys.call()
	require_a('named collection', coll, pcall)

	if (length(coll) == 0) {
		list()
	} else {
		xZip( names(coll), unname(coll) )
	}
}
