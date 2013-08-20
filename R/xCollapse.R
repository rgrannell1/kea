
#' Collapse a character vector into a string with a delimiter.
#'
#' @param delimiter a length-one character vector.
#' @param strings a character vector.
#'
#' @return a string.
#'
#' @export

#| function: xCollapse version: 0.1 finished: false

xCollapse <- function (delimiter, strings) {
	# string -> strings -> string
	# Collapse a collection of strings into
	# a string with by a delimiter.

	pcall <- sys.call()
	require_a("string", delimiter, pcall)
	require_a("character", strings, pcall)

	paste0(strings, collapse = delimiter)

}
