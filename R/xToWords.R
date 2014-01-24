
#' xToWords
#'
#' Split a str at whitespace.
#'
#' @details
#'    The input string is split at spaces, tabs, and newlines,
#'    or a mix of the above. Multiple whitespaces are treated
#'    the same as one whitespace.
#'
#' @param
#'    str a length-one character vector. The string to split into
#'    words.
#'
#' @return
#'    A character vector, with one or more elements.
#'
#' @family character_functions
#'
#' @rdname xToWords
#' @export

xToWords <- function (str) {
	# str -> Vector str
	# split a str at every whitespace character, returning
	# a character vector of equal or greater length.

	invoking_call <- sys.call()

	assert(
		!missing(str), invoking_call,
		exclaim$parametre_missing(str))

	insist$must_be_collection(str, invoking_call)
	str <- as_typed_vector(str, 'character')

	if (nchar(str) == 0 || length(str) == 0) {
		character(0)
	} else {
		strsplit(str, split = '[ \n\t]+')[[1]]
	}
}
