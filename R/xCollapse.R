
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


#' Collapse a character vector into a str with a delim.
#'
#' @param delim a length-one character vector.
#' @param strs a character vector.
#'
#' @return a str.
#'
#' @export

#| function: xCollapse version: 0.1 finished: false

xCollapse <- xAutoPartial(function (delim, strs) {
	# string -> Vector string -> string
	# Collapse a collection of strs into
	# a str with by a delim.

	pcall <- sys.call()
	require_a("string", delim, pcall)
	require_a("character", strs, pcall)

	if (length(strs) == 0) {
		character(0)
	} else {
		paste0(strs, collapse = delim)		
	}
})
