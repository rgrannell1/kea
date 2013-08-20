
#' Collapase a character vector with newlines as delimiters.
#'
#' @param strings a character vector.
#'
#' @return a string.
#'
#' @export

#| function: xUnlines version: 0.1 finished: false

xUnlines <- function (strings) {
	# [string] -> string
	# collapse the collection strings with a newline.

	pcall <- sys.call()
	require_a("character", strings, pcall)

	paste0(strings, collapse = '\n')
}