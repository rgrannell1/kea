
#' Generate a list of n-element lists from a list of n colls.
#' 
#' @param colls a list or pairlist of n-vectors, lists or pairlists.
#'
#' @return returns a list of equal length to the shortest input collection,
#'	 where each element is a n-element list.
#' @section Corner Cases:
#'	 the empty list is returned if the shortest collection has length-zero, or 
#'	 \code{collection} is the empty list.
#'	 Each collection is truncated to the length of the shortest collection.
#'
#' @export

#| function: xUnzip version: 0.1 finished: false

xUnzip <- function (colls) {
	xUnzipWith(fn = function (...) list(...), colls)
}
