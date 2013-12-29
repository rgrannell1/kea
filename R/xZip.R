
#' xZip
#'
#' Generate a list of n-element lists from n collections.
#'
#' @param
#'    colls n-vectors, lists or pairlists.
#'
#' @return
#'    returns a list of equal length to the shortest
#'    input collection, with each element being an n-element list.
#'
#' @section Corner Cases:
#'    the empty list is returned if the shortest collection
#'    has length-zero, or no collections are included. Each
#'    collection is truncated to the length of the shortest collection.
#'
#' @family collection_functions
#'
#' @family reshaping_functions
#'
#' @family variadic_functions
#'
#' @rdname xZip
#' @export

xZip <- function (colls) {
	xZipWith(function (...) colls, colls)
}

#' @rdname xZip
#' @export

xZip... <- function (...) {
	xZip(list(...))
}
