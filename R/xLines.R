
#' xLines
#' 
#' Split a string at every newline character.
#'
#' @param str a string.
#'
#' @return a character vector.
#'
#' @template glossary
#'
#' @examples inst/examples/blank.R
#' @export

xLines <- function (str) {
	# str -> Vector str
	# split str at every newline, returning 
	# a character vector of equal or greater length.
	
	pcall <- sys.call()

	assert(
		!missing(str), pcall,
		exclaim$parameter_missing(str))

	assert(
		is.character(str), pcall,
		exclaim$must_be_character(str))
	
	assert(
		length(str) %in% c(0, 1), pcall)

	if (length(str) == 0) {
		character(0)
	} else if (nchar(str) == 0) {
		""
	} else {
		strsplit(str, split = "\n+")[[1]]
	}
}
