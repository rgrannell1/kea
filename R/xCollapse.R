
#' xCollapse
#' 
#' Concatenate a character vector into a string with a delimiter.
#'
#' @param str a string to use as a delimiter.
#' @param ... a number of character vectors.
#'
#' @return a length-one character vector.
#'
#'
#' @template glossary
#'
#' @example inst/examples/blank.R
#' @export

xCollapse <- function (str, ...) {
	# string -> Vector string -> string
	# Collapse a collection of strs into
	# a str with by a delim.

	pcall <- sys.call()
	
	strs <- lapply(list(...), dearrowise)

	assert(
		!missing(str), pcall,
		exclaim$parameter_missing(str))

	str <- dearrowise(str)

	assert(
		is_collection(str), pcall,
		exclaim$must_be_collection(str))

	str <- coerce_to_typed_vector(str, 'character')

	assert(
		is_collection(strs), pcall,
		exclaim$must_be_collection(strs))

	strs <- coerce_to_typed_vector(strs, 'character')

	assert(
		length(str) %in% c(0, 1),
		exclaim$must_have_length( str, c(0, 1)) )

	if (length(strs) == 0) {
		character(0)
	} else {
		if (length(str) == 0) {
			str <- ""
		}
		paste0(strs, collapse = str)		
	}
}
