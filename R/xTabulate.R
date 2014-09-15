
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
#'     if sorting is required \bold{xSortBy} can be used.
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
#'    Variadic
#'
#' @example
#'    inst/examples/example-xTabulate.R
#'
#' @family reshaping_functions
#'
#' @rdname xTabulate
#' @export

xTabulate <- MakeFun('xTabulate', function (coll) {
	cTabulate(coll)
})

#' @rdname xTabulate
#' @export

xTabulate_ <- MakeVariadic(xTabulate, 'coll')
