
#' Collapse a character vector into a string with a delim.
#'
#' @param delim a length-one character vector.
#' @param strings a character vector.
#'
#' @return a string.
#'
#' @export

#| function: xCollapse version: 0.1 finished: false

xCollapse <- function (delim, strings) {
	# string -> Vector string -> string
	# Collapse a collection of strings into
	# a string with by a delim.

	pcall <- sys.call()
	require_a("string", delim, pcall)
	require_a("character", strings, pcall)

	paste0(strings, collapse = delim)

}
