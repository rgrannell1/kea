
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
#' @examples inst/examples/blank.R
#' @export

xGet <- function (str) {
	# Vector string -> (Collection -> [any])

	pcall <- sys.call()
	
	assert(
		!missing(str), pcall,
		exclaim$parameter_missing(str))

	assert(
		is.character(str), pcall,
		exclaim$must_be_character(str))

	assert(
		length(str) %in% c(0, 1), pcall,
		exclaim$must_have_length( str, c(0, 1)) )

	str <- coerce_to_vector(str, 'character')

	if (length(str) == 0) {
		str <- ""
	}
	function (coll) {
		as.list( coll[[str]] )
	}
}
