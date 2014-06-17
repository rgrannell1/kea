
#' xThirdAs
#'
#' Set the third value in a collection.
#'
#' @section Type Signature:
#'     any -> |any| -> [any]
#'
#' @param
#'    val an arbitrary value. The value to use.
#'
#' @param
#'    coll a collection. The collection to set the
#'    third element of.
#'
#' @param
#'    ... see above.
#'
#' @return
#'    A collection.
#'
#' @section Corner Cases:
#'     Throws an error for collections shorter than three
#'     elements.
#'
#' @template
#'    Variadic
#'
#' @example
#'    inst/examples/example-xThirdAs.R
#'
#' @rdname xThirdAs
#' @export

xThirdAs <- MakeFun(function (val, coll) {

	MACRO( Must_Be_Longer_Than(2, coll) )

	coll      <- as.list(coll)
	coll[[3]] <- val

	coll

})

#' @rdname xThirdAs
#' @export

xThirdAs_ <- MakeVariadic(xThirdAs, 'coll')
