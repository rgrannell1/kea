
#' xZip
#'
#' Transpose a collection of collections.
#'
#' @section Type Signature:
#'     ||any|| -> ||any||
#'
#' @details
#'    \bold{xZip} converts the 'columns' of a collection of
#'    collections (each inner collection) to 'rows.'
#'
#'    \code{coll <- list( list(1, 2, 3), list('a', 'b', 'c') )}
#'
#'    \code{xZip(coll)}
#'
#'    \code{list( list(1, 'a'), list(2, 'b'), list('c') )}
#'
#'    In the above case the 'columns' - a list of numbers and
#'    a list of letters - were zipped into corresponding 'rows'
#'    of a number and a letter.
#'
#'    \bold{xZip} is an involution - a function that is its own
#'    inverse. For any collection, \bold{xZip(xZip(coll))}
#'    is the original collection.
#'
#'    Applying \bold{xZip} again will reconvert the 'rows'
#'    back into columns.
#'
#'    \code{coll <- list( list(1, 2, 3), list('a', 'b', 'c') )}
#'
#'    \code{xZip(coll)}
#'
#' @param
#'    colls a collection of collections of equal lengths.
#'    The collections to zip together.
#'
#' @param
#'    ... see above.
#'
#' @return
#'    Returns a list of lists.
#'
#' @section Corner Cases:
#'    The empty list is returned if the shortest collection
#'    has length-zero, or no collections are included.
#'
#' @family reshaping_functions
#'
#' @template
#'    C++
#'
#' @template
#'    Variadic
#'
#' @example
#'    inst/examples/example-xZip.R
#'
#' @rdname xZip
#' @export

xZip <- MakeFun(function (colls) {
	cZip(colls)
})

#' @rdname xZip
#' @export

xZip_ <- MakeVariadic(xZip, 'colls')
