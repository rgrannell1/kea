
#' xGet
#'
#' Return a function that selects a key from a collection.
#'
#' @param
#'    str a string.
#'
#' @return
#'    a unary function that takes a collection.
#'
#' @section Corner Cases:
#'     returns the empty list if \code{coll} is length-zero.
#'
#' @export

xGet <- function (str) {
	# Vector string -> (Collection -> [any])
	# Return a function that selects a key from a collection.

	invoking_call <- sys.call()

	assert(
		!missing(str), invoking_call,
		exclaim$parameter_missing(str))

	assert(
		is.character(str), invoking_call,
		exclaim$must_be_character(
			str, profile_object(str)) )

	assert(
		length(str) %in% 0:1, invoking_call,
		exclaim$must_have_length(
			str, 0:1, profile_object(str)) )

	str <- as_typed_vector(str, 'character', True)

	function (coll) {
		unname(as.list( coll )[names(coll) == str])
	}
}
