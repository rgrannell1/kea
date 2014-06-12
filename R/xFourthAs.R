
#' xFourthAs
#'
#' Set the fourth value in a collection.
#'
#' @section Type Signature:
#'     any -> |any| -> [any]
#'
#' @param
#'    val an arbitrary value. The value to use.
#'
#' @param
#'    coll a collection. The collection to set the
#'    fourth element of.
#'
#' @param
#'    ... see above.
#'
#' @return
#'    A collection.
#'
#' @section Corner Cases:
#'     Throws an error for collections shorter than four
#'     elements.
#'
#' @template
#'    Variadic
#'
#' @example
#'    inst/examples/example-xFourthAs.R
#'
#' @rdname xFourthAs
#' @export

xFourthAs <- MakeFun(function (val, coll) {

	MACRO( Must $ Be_Longer_Than(3, coll) )

	coll <- as.list(coll)
	coll[[4]] <- val
	coll

})

#' @rdname xFourthAs
#' @export

xFourthAs_ <- MakeVariadic(xFourthAs, 'coll')
