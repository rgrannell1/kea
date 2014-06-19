
#' xFourthOf
#'
#' @section Type Signature:
#'     |any| -> any
#'
#' Return the fourth value in a collection.
#'
#' @param
#'    coll a collection. The collection to get the
#'    fourth element of.
#'
#' @param
#'    ... see above.
#'
#' @return
#'    The fourth value in \bold{coll}.
#'
#' @section Corner Cases:
#'    Throws an error if \bold{coll} has less than four element; this is
#'    because any other corner case would violate the functions type-signature.
#'
#' @family selection_functions
#'
#' @template
#'    Variadic
#'
#' @rdname xFourthOf
#' @export

xFourthOf <- MakeFun(function (coll) {


	MACRO( Must_Be_Longer_Than(3, coll) )

	coll[[4]]
})

#' @rdname xFourthOf
#' @export

xFourthOf_ <- MakeVariadic(xFourthOf, 'coll')
