
#' xUnzipIndices
#'
#' Split a named collection into a list of name: value list pairs.
#'
#' @section Type Signature:
#'     |any| -> [[ <number>, ...any ]]
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

	MACRO( Must $ Not_Be_Missing(coll) )
	MACRO( Must $ Be_Collection(coll) )

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
