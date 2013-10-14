
#' xAssoc
#' 
#' Convert a list of name : value collections into a named list.
#'
#' @param coll a list or pairlist of list or pairlist pairs, with the first element being a 
#'	string and the second element being any value.
#'
#' @return a named list.
#'
#' @section Corner Cases: 
#'     returns the empty list if \code{coll} is length-zero.
#' @template glossary
#'
#' @example inst/examples/blank.R
#' @export

xAssoc <- function (coll) {
	# Collection Collection any -> [any]

	pcall <- sys.call()

	assert(
		!missing(coll), pcall, 
		exclaim$parameter_missing(coll))
	
	assert(
		is.recursive(coll), pcall,
		exclaim$must_be_recursive(coll))
	
	assert(
		all(sapply(coll, length) == 2), pcall,
		exclaim$must_be_collection_of_length(coll, 2))

	if (length(coll) == 0) {
		list()
	} else {
		keys <- vapply(
			coll,
			function (elem) {
				key <- elem[[1]]

				assert(
					is.character(key) && length(key) == 1, pcall,
					exclaim$must_be_string(key))
				
				key
			},
			character(1), USE.NAMES = False)

		structure(
			Map( function (elem) elem[[2]], coll ),
			names = keys)		
	}
}








