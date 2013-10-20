
#' xChars
#' 
#' Split a single string into a vector of characters.
#'
#' @param str a length-one character vector.
#'
#' @return a character vector of length \code{nchar(str)}.
#'
#' @template glossary
#'
#'
#' @family higher_order_function
#'
#' @example inst/examples/blank.R
#' @export

xChars <- function (str) {
	# str -> Vector str
	# split str at every character, returning 
	# a character vector of equal or greater length.
	
	pcall <- sys.call()	

	assert(
		!missing(str), pcall,
		exclaim$parameter_missing(str))

	str <- dearrowise(str)

	assert(
		length(str) %in% c(0, 1), pcall,
		exclaim$must_have_length( str, c(0, 1)) )

	str <- coerce_to_typed_vector(str, 'character')

	if (length(str) == 0) {
		character(0)
	} else if (nchar(str) == 0) {
		""
	} else {
		strsplit(str, "")[[1]]
	}
}
