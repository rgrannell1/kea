
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
#' @examples inst/examples/blank.R
#' @export

xCollapse <- function (str, ...) {
	# string -> Vector string -> string
	# Collapse a collection of strs into
	# a str with by a delim.

	pcall <- sys.call()
	
	assert(
		!missing(str), pcall)
	
	assert(
		is.character(str), pcall)
	assert(
		length(str) %in% c(0, 1))

	strs <- c(...)
	
	assert(is.vector(strs) || is.pairlist(strs))
	assert(
		length(strs) == 0 || is.character(unlist(strs)), pcall)

	if (length(strs) == 0) {
		character(0)
	} else {
		if (length(str) == 0) {
			str <- ""
		}
		paste0(strs, collapse = str)		
	}
}
