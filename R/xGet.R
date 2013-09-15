
#' xGet
#' 
#' Return a function that selects a key from a collection.
#'
#' @param str a string.
#'
#' @return a unary function that takes a collection.
#'
#' @section Corner Cases: 
#'     returns the empty list if \code{coll} is length-zero.
#' @template glossary
#'
#' @examples 
#' @export

xGet <- function (str) {
	# Vector string -> (Collection -> [any])

	pcall <- sys.call()
	
	assert(
		is.character(str) && length(str) == 1, pcall)

	function (coll) {
		as.list( coll[[str]] )
	}
}
