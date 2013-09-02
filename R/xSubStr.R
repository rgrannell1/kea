
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
#'     returns the empty list if \code{coll is length-zero}.
#' @template glossary
#'
#' @examples 
#' @export

#| function: xSubStr version: 0.1 finished: false 

xSubStr <- function (str, inds) {
	# str -> integer -> Vector str
	# subset a str using normal R vector indexing.
	
	pcall <- sys.call()

	require_a(c("length_zero character", "string"), str, pcall)
	require_a(c('length_zero double', 'length_zero integer', 'whole'), inds, pcall)
	
	if (length(str) == 0) {
		character(0)
	} else if (length(inds) == 0) {
		str
	} else {
		if (max(inds) > nchar(str)) {
			stop('out of bounds')
		}

		chars <- strsplit(str, "")[[1]]
		paste0(chars[inds], collapse = "")
	}
}
