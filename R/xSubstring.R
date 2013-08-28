
#' xSubString
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

#| function: xSubString version: 0.1 finished: false 

xSubString <- function (str, inds) {
	# str -> integer -> Vector str
	# subset a str using normal R vector indexing.
	
	pcall <- sys.call()

	require_a("string", str, pcall)
	require_a("numeric", inds, pcall)
	
	chars <- strsplit(str, "")[[1]]
	paste0(chars[inds], collapse = "")		
}
