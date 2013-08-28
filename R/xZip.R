
#' Generate a list of n-element lists from n collections.
#' 
#' @param ... n-vectors, lists or pairlists.
#'
#' @return returns a list of equal length to the shortest input collection, with each element being
#'	 an n-element list.
#' @section Corner Cases:
#'	  the empty list is returned if the shortest collection has length-zero, or no collections
#'	  are included. Each collection is truncated to the length of the shortest collection.
#'
#' @export

xZip <- function(...) {
	xZipWith(function (...) list(...), ...)
}
