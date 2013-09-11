
#' xAssoc
#' 
#' Convert a list of name:value lists into a named list.
#'
#' @param coll a list or pairlist of list or pairlist pairs, with the first element being a 
#'	string and the second element being any value.
#'
#' @return a named list.
#'
#' @section Corner Cases: 
#'     returns the empty list if \code{coll is length-zero}.
#' @template glossary
#'
#' @examples 
#' @export

xAssoc <- function (coll) {
	# Collection Collection any -> [any]

	pcall <- sys.call()
	require_a('recursive_of_length_two', coll, pcall)

	if (length(coll) == 0) {
		list()
	} else {
		keys <- vapply(
			coll,
			function (elem) {
				key <- elem[[1]]
				stopifnot(is.character(key) && length(key) == 1)
				key
			},
			character(1), USE.NAMES = False)

		structure(
			Map( function (elem) elem[[2]], coll ),
			names = keys)		
	}
}
