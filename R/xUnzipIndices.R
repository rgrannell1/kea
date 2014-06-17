
#' xUnzipIndices
#'
#' Split a collection into a list of index: value pairs
#'
#' @section Type Signature:
#'     |any| -> [[ <number>, ...any ]]
#'
#' @details
#'     \bold{xUnzipIndices} reshapes a collection to allow you to pass
#'     both the indices and elements to another higher order function.
#'     More generally, \bold{xUnzipIndices} allows you to selectively work on
#'     elements based on their position in a collection, which functions like
#'     \bold{xMap} and \bold{xSelect} otherwise do not support.
#'
#' @param
#'    coll a collection. The collection to split into index, element pairs.
#'
#' @param
#'    ... see above.
#'
#' @return
#'    A named list.
#'
#' @section Corner Cases:
#'      Returns \code{list()} if \code{coll} is length-zero.
#'
#' @family reshaping_functions
#'
#' @family key_functions
#'
#' @template
#'    Variadic
#'
#' @example
#'    inst/examples/example-xUnzipIndices.R
#'
#' @rdname xUnzipIndices
#' @export

xUnzipIndices <- MakeFun(function (coll) {

	if (length(coll) == 0) {
		list()
	} else {
		lapply(seq_along(coll), function (ith) {
			list(ith, coll[[ith]] )
		})
	}
})

#' @rdname xUnzipIndices
#' @export

xUnzipIndices_ <- MakeVariadic(xUnzipIndices, 'coll')
