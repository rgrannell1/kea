#' xDigits
#'
#' Split a single string into a vector of characters.
#'
#' @param str a length-one character vector.
#'
#' @return a character vector of length \code{nchar(str)}.
#'
#' @example inst/examples/blank.R
#' @export

xDigits <- function (num) {
	# num -> Vector num
	# split str at every character, returning
	# a character vector of equal or greater length.

	parent_call <- sys.call()

	assert(
		!missing(num), parent_call,
		exclaim$parameter_missing(num))

	num <- dearrowise(num)

	assert(
		length(num) %in% 0:1, parent_call,
		exclaim$must_have_length( num, 0:1) )

	num <- coerce_to_typed_vector(num, 'numeric')

	if (length(num) == 0) {
		numeric(0)
	} else {
		as.numeric( strsplit(toString(num), "")[[1]] )
	}
}

