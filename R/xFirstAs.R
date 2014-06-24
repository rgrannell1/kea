
#' xFirstAs
#'
#' Set the first value in a collection.
#'
#' @section Type Signature:
#'     any -> |any| -> [any]
#'
#' @param
#'    val an arbitrary value. The value to set.
#'
#' @param
#'    coll a collection. The collection to set the
#'    first element of.
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
#'    inst/examples/example-xFirstAs.R
#'
#' @rdname xFirstAs
#' @export

xFirstAs <- MakeFun('xFirstAs', function (val, coll) {

	MACRO( Must_Be_Longer_Than(0, coll) )

	coll      <- as.list(coll)
	coll[[1]] <- val
	coll

})

#' @rdname xFirstAs
#' @export

xFirstAs_ <- MakeVariadic(xFirstAs, 'coll')
