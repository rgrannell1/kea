
#' xLastAs
#'
#' Set the last value in a collection.
#'
#' @section Type Signature:
#'     any -> |any| -> [any]
#'
#' @param
#'    val an arbitrary value. The value to use.
#'
#' @param
#'    coll a collection. The collection to set the
#'    last element of.
#'
#' @param
#'    ... see above.
#'
#' @return
#'    A collection.
#'
#' @section Corner Cases:
#'     Throws an error for collections shorter than one
#'     element.
#'
#' @template
#'    Variadic
#'
#' @example
#'    inst/examples/example-xLastAs.R
#'
#' @rdname xLastAs
#' @export

xLastAs <- MakeFun(function (val, coll) {

	MACRO( Must $ Be_Collection(coll) )
	MACRO( Must $ Be_Longer_Than(0, coll) )

	coll <- as.list(coll)
	coll[[ length(coll) ]] <- val
	coll

})

#' @rdname xLastAs
#' @export

xLastAs_ <- MakeVariadic(xLastAs, 'coll')
