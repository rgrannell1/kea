
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
#'
#'
#' @example inst/examples/blank.R
#' @export

xGet <- function (str) {
	# Vector string -> (Collection -> [any])
	# Return a function that selects a key from a collection.

	parent_call <- sys.call()

	assert(
		!missing(str), parent_call,
		exclaim$parameter_missing(str))

	str <- dearrowise(str)

	assert(
		is.character(str), parent_call,
		exclaim$must_be_character(str))

	assert(
		length(str) %in% 0:1, parent_call,
		exclaim$must_have_length( str, 0:1) )

	str <- as_typed_vector(str, 'character', True)

	function (coll) {
		unname(as.list( coll )[names(coll) == str])
	}
}
