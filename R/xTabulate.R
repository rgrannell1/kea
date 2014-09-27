
#' xTabulate
#'
#' Tabulate a collection into pairs of value:frequency lists.
#'
#' @section Type Signature:
#'    |any| -> ||any, <number>||
#'
#' @details
#'     \bold{xTabulate} is superficially similar to the base
#'     function \bold{table}: given a collection that may or
#'     may not contain duplicates, it calculates the frequencies
#'     of each unique element.
#'
#'     \code{xTabulate(c('y', 'n', 'y', 'y', 'n'))}
#'
#'     \code{list(list("y", 3), list("n", 2))}
#'
#'     The result of the tabulation is unsorted for efficiency;
#'     if sorting is required \bold{xSortBy} can be used. The performance of
#'     \bold{xTabulate} heavily depends on the number of unique groups in the output;
#'     the worst case is O(n^2), when there are no duplicates in the input collection. The
#'     best case performance is when there are only duplicates in the input collection.
#'
#'
#' @param
#'    coll a collection. The values to find the frequency of.
#'
#' @param
#'    ... see above.
#'
#' @return
#'    A list.
#'
#' @section Corner Cases:
#'    If \bold{coll} is length-zero the empty list is returned.
#'
#' @template
#'    C++
#'
#' @template
#'    Variadic
#'
#' @example
#'    inst/examples/example-xTabulate.R
#'
#' @family reshaping_functions
#'
#' @rdname xTabulate
#' @export

xTabulate <- MakeFun(function (coll) {
	cTabulate(coll)
})

#' @rdname xTabulate
#' @export

xTabulate_ <- MakeVariadic(xTabulate, 'coll')
