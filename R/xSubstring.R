
#' Subset a string using normal R vector indexing.
#'
#' @param string a length-one character vector.
#' @param indices a collection of valid indices of the characters in \code{string}.
#' @return a length-one character vector.
#'
#' @section Corner Cases:
#'    throws an error if the largest index is larger than the number of characters in 
#'    the input string.
#' @export

#| function: xSubstring version: 0.1 finished: false 

xSubstring <- function (string, indices) {
	# string -> integer -> Vector string
	# subset a string using normal R vector indexing.
	
	pcall <- sys.call()

	require_a("string", string, pcall)
	require_a("numeric", indices, pcall)
	
	if (max(indices) > nchar(string)) {
		stop("largest index larger than number of characters in string")
	} else {
		chars <- strsplit(string, "")[[1]]
		paste0(chars[indices], collapse = "")		
	}
}
