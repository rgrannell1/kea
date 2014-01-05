
#' xChars
#'
#' Split a single string into a vector of characters.
#'
#' @param
#'    str a length-one character vector.
#'
#' @return
#'    a character vector of length \code{nchar(str)}, with each element being a
#'    single character.
#'
#' @family character_functions
#'
#' @rdname xChars
#' @export

xChars <- function (str) {
	# str -> Vector str
	# split str at every character, returning
	# a character vector of equal or greater length.

	invoking_call <- sys.call()

	assert(
		!missing(str), invoking_call,
		exclaim$parametre_missing(str))

	str <- as_typed_vector(str, 'character')

	assert(
		length(str) %in% 0:1, invoking_call,
		exclaim$must_have_length(
			str, 0:1, summate(str)) )

	if (length(str) == 0 || nchar(str) == 0) {
		character(0)
	} else {
		strsplit(str, "")[[1]]
	}
}
