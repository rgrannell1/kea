
#' xGather
#'
#' Group adjacent equal elements into sublists.
#'
#' @section Type Signature:
#'     |any| -> [[any]]
#'
#' @param
#'     coll a collection. The elements to group.
#'
#' @param
#'     ... see above.
#'
#' @return
#'     A list.
#'
#' @section Corner Cases:
#'    If \bold{coll} is a empty collection the empty list is returned.
#'
#' @template
#'    Variadic
#'
#' @family reshaping_functions
#'
#' @example
#'    inst/examples/example-xGather.R
#'
#' @rdname xGather
#' @export

xGather <- function (coll) {
	cGather(coll)
}

#' @rdname xGather
#' @export

xGather_ <- MakeVariadic(xGather, 'coll')
