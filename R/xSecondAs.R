
#' xSecondAs
#'
#' Set the second value in a collection.
#'
#' @section Type Signature:
#'     any -> |any| -> [any]
#'
#' @param
#'    val an arbitrary value. The value to set.
#'
#' @param
#'    coll a collection. The collection to set the
#'    second element of.
#'
#' @param
#'    ... see above.
#'
#' @return
#'    A collection.
#'
#' @section Corner Cases:
#'     Throws an error for collections shorter than two
#'     elements.
#'
#' @template
#'    Variadic
#'
#' @example
#'    inst/examples/example-xSecondAs.R
#'
#' @rdname xSecondAs
#' @export

xSecondAs <- MakeFun('xSecondAs', function (val, coll) {

	MACRO( Must_Be_Longer_Than(1, coll) )

	coll      <- as.list(coll)
	coll[[2]] <- val

	coll

})

#' @rdname xSecondAs
#' @export

xSecondAs_ <- MakeVariadic(xSecondAs, 'coll')
