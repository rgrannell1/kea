
#' xShuffle
#'
#' Randomly rearrange a collection.
#'
#' @section Type Signature:
#'     |any| -> [any]
#' @param
#'      coll a collection. The collection to rearrange.
#'
#' @param
#'    ... see above.
#'
#' @return
#'      A list.
#'
#' @section Corner Cases:
#'      Returns the empty list if \bold{coll} is length-zero.
#'
#' @family reshaping_functions
#' @family inpure_functions
#'
#' @template
#'    Variadic
#'
#' @example
#'    inst/examples/example-xShuffle.R
#'
#' @rdname xShuffle
#' @export

xShuffle <- MakeFun('xShuffle', function (coll) {

	if (length(coll) == 0) {
		keep_names(list(), coll)
	} else {
		as.list(rsample(coll))
	}
})

#' @rdname xShuffle
#' @export

xShuffle_ <- MakeVariadic(xShuffle, 'coll')
