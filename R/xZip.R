
#' xZip
#'
#' Generate a list of n-element lists from n collections.
#'
#' @param
#'    colls n-vectors, lists or pairlists.
#'
#' @param
#'    ... see above.
#'
#' @return
#'    Returns a list.
#'
#' @section Corner Cases:
#'    The empty list is returned if the shortest collection
#'    has length-zero, or no collections are included.
#'
#' @family reshaping_functions
#'
#' @template
#'    Variadic
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
