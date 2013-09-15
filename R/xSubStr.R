
#' xSubStr
#' 
#' Subset a string using normal R vector indexing.
#'
#' @param str a string.
#' @param inds indices of \code{str}.
#'
#' @return a character vector.
#'
#' @section Corner Cases: 
#'     returns the empty list if \code{coll} is length-zero.
#' @template glossary
#'
#' @examples 
#' @export

xSubStr <- function (str, inds) {
	# str -> integer -> Vector str
	# subset a str using normal R vector indexing.
	
	pcall <- sys.call()

	assert(
		is.character(str) && length(str) < 2, pcall)
	assert(
		is.numeric(inds) && all(round(inds) == inds), pcall)
	
	if (length(str) == 0) {
		character(0)
	} else if (length(inds) == 0) {
		str
	} else {
		assert(
			max(inds) <= nchar(str))

		chars <- strsplit(str, "")[[1]]
		paste0(chars[inds], collapse = "")
	}
}
