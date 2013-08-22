
#' Subset a str using normal R vector indexing.
#'
#' @param str a length-one character vector.
#' @param iths a collection of valid iths of the characters in \code{str}.
#' @return a length-one character vector.
#'
#' @section Corner Cases:
#'    throws an error if the largest index is larger than the number of characters in 
#'    the input str.
#' @export

#| function: xSubstr version: 0.1 finished: false 

xSubstr <- function (str, iths) {
	# str -> integer -> Vector str
	# subset a str using normal R vector indexing.
	
	pcall <- sys.call()

	require_a("string", str, pcall)
	require_a("numeric", iths, pcall)
	
	if (max(iths) > nchar(str)) {
		stop("largest index larger than number of characters in str")
	} else {
		chars <- strsplit(str, "")[[1]]
		paste0(chars[iths], collapse = "")		
	}
}
