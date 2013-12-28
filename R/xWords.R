
#' xWords
#'
#' Split a str at whitespace.
#'
#' @param
#'    str a length-one character vector.
#'
#' @return
#'    a character vector, with one or more elements.
#'
#' @family character_vector_functions
#'
#' @export

xWords <- function (str) {
	# str -> Vector str
	# split a str at every whitespace character, returning
	# a character vector of equal or greater length.

	invoking_call <- sys.call()

	assert(
		!missing(str), invoking_call,
		exclaim$parametre_missing(str))

	str <- as_typed_vector(str, 'character', True)

	assert(
		length(str) %in% 0:1, invoking_call,
		exclaim$must_have_length(
			str, 0:1, profile_object(str)) )

	if (nchar(str) == 0 || length(str) == 0) {
		character(0)
	} else {
		strsplit(str, split = '[ \n\t]+')[[1]]
	}
}
