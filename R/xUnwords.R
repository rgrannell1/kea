
#' Collapase a character vector with spaces as delimiters.
#'
#' @param strings a character vector, or list or pairlist of strings.
#'
#' @return a string.
#'
#' @export

#| function: xUnwords version: 0.1 finished: false

xUnwords <- function (strings) {
	# [string] -> string
	# collapse the collection strings with a space.

	pcall <- sys.call()
	require_a("character", strings, pcall)

	paste0(strings, collapse = ' ')
}