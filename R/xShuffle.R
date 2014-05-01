
#' xShuffle
#'
#' Randomly rearrange a collection.
#'
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
#'
#' @template
#'    Variadic
#'
#' @example
#'    inst/examples/example-xShuffle.R
#'
#' @rdname xShuffle
#' @export

xShuffle <- MakeFun(function (coll) {

	MACRO( Must $ Not_Be_Missing(coll) )
	MACRO( Must $ Be_Collection(coll) )

	if (length(coll) == 0) {
		list()
	} else if (length(coll) == 1)
		as.list(coll)
	else {
		as.list(sample(coll))
	}
})

#' @rdname xShuffle
#' @export

xShuffle_ <- MakeVariadic(xShuffle, 'coll')
