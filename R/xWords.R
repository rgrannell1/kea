
#' Split a str at whitespace.
#'
#' @param str a length-one character vector.
#'	 
#' @return a character vector, with one or more elements.
#'
#' @export

xWords <- function (str) {
	# str -> Vector str
	# split a str at every whitespace character, returning 
	# a character vector of equal or greater length.
	
	pcall <- sys.call()

	assert(
		!missing(str), pcall,
		exclaim$parameter_missing(str))

	str <- dearrowise(str)

	str <- coerce_to_vector(str, 'character')

	assert(
		length(str) %in% c(0, 1), pcall,
		exclaim$must_have_length(str, c(0, 1)))

	if (nchar(str) == 0) {
		""
	} else {
		strsplit(str, split = '[ \n\t]+')[[1]]
	}
}
