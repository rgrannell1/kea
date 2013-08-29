
#' xCollapse
#' 
#' Concatenate a character vector into a string with a delimiter.
#'
#' @param delim a string to use as a delimiter.
#' @param strs a character vector of strings.
#'
#' @return a length-one character vector.
#'
#'
#' @template glossary
#'
#' @examples 
#' @export

#| function: xCollapse version: 0.1 finished: false

xCollapse <- function (delim, strs) {
	# string -> Vector string -> string
	# Collapse a collection of strs into
	# a str with by a delim.

	pcall <- sys.call()
	require_a("string", delim, pcall)
	require_a("collection_of_length_one", strs, pcall)

	strs <- unlist(strs)

	require_a("character", strs, pcall)

	if (length(strs) == 0) {
		character(0)
	} else {
		paste0(strs, collapse = delim)		
	}
}
