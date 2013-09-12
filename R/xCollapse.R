
#' xCollapse
#' 
#' Concatenate a character vector into a string with a delimiter.
#'
#' @param delim a string to use as a delimiter.
#' @param ... a number of character vectors.
#'
#' @return a length-one character vector.
#'
#'
#' @template glossary
#'
#' @examples 
#' @export

xCollapse <- function (delim, ...) {
	# string -> Vector string -> string
	# Collapse a collection of strs into
	# a str with by a delim.

	pcall <- sys.call()
	
	assert(
		is.character(delim) && length(delim) == 1, pcall)
	
	strs <- c(...)
	
	assert(
		is.vector(strs) || is.pairlist(strs) && is.character(unlist(strs)), pcall)

	if (length(strs) == 0) {
		character(0)
	} else {
		paste0(strs, collapse = delim)		
	}
}
