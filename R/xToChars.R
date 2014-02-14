
#' xToChars
#'
#' Split a single string into a vector of characters.
#'
#' @param
#'    str a length-one character vector. The string to split into
#'    characters.
#'
#' @return
#'    A character vector of length \code{nchar(str)}, with each element being a
#'    single character.
#'
#' @family character_functions
#'
#' @example
#'    inst/examples/example-xToChars.R
#'
#' @rdname xToChars
#' @export

xToChars <- function (str) {
	# str -> Vector str
	# split str at every character, returning
	# a character vector of equal or greater length.

	invoking_call <- sys.call()

	insist $ must_not_be_missing(str)

	insist $ must_be_collection (str, invoking_call)
	str <- as_typed_vector(str, 'character')

	if (length(str) == 0 || nchar(str) == 0) {
		character(0)
	} else {
		chars <- strsplit(str, "")[[1]]
		chars[nchar(chars) > 0]
	}
}
