
#' Collapase a character vector with newlines as delimiters.
#'
#' @param strings a character vector.
#'
#' @return a length-one character vector.
#'
#' @export

#| function: xUnlines version: 0.1 finished: false

xUnlines <- function (strings) {
	# Collection string -> string;
	# collapse the collection of strings with a newline.

	pcall <- sys.call()
	require_a("collection_of_length_one", strings, pcall)

	strings <- unlist(strings)
	require_a("character", strings, pcall)

	paste0(strings, collapse = '\n')
}