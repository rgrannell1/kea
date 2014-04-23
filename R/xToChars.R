
#' xToChars
#'
#' Split a single string into a vector of characters.
#'
#' @section Type Signature:
#'     <character> -> <character>
#'
#' @param
#'    str a length-one character vector. The string to split into
#'    characters.
#'
#' @return
#'    A character vector of length \code{nchar(str)}, with each element being a
#'    single character.
#'
#' @family text_processing_functions
#'
#' @example
#'    inst/examples/example-xToChars.R
#'
#' @rdname xToChars
#' @export

xToChars <- MakeFun(function (str) {
	# str -> Vector str
	# split str at every character, returning
	# a character vector of equal or greater length.

	MACRO( Must $ Not_Be_Missing(str) )
	MACRO( Must $ Be_Collection(str) )

	str <- as_typed_vector(str, 'character')

	if (length(str) == 0 || nchar(str) == 0) {
		character(0)
	} else {
		chars <- strsplit(str, "")[[1]]
		chars[nchar(chars) > 0]
	}
})
