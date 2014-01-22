
#' xToLines
#'
#' Split a string at every newline character.
#'
#' @details
#'    The input string is split at newline characters.
#'    Multiple newlines are treated the same as one newline.
#'
#' @param
#'    str a string.
#'
#' @return
#'    A character vector.
#'
#' @family character_functions
#'
#' @rdname xToLines
#' @export

xToLines <- function (str) {
	# str -> Vector str
	# split str at every newline, returning
	# a character vector of equal or greater length.

	invoking_call <- sys.call()

	assert(
		!missing(str), invoking_call,
		exclaim$parametre_missing(str))

	insist$must_be_collection(str, invoking_call)
	str <- as_typed_vector(str, 'character')

	if (length(str) == 0 || nchar(str) == 0) {
		character(0)
	} else {
		strsplit(str, split = "\n+")[[1]]
	}
}
