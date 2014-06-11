
#' xLastOf
#'
#' Return the last element in a collection.
#'
#' @section Type Signature:
#'     |any| -> any
#'
#' @param
#'    coll a collection. The collection to return
#'    the last element of.
#'
#' @param
#'    ... see above.
#'
#' @return
#'    The value of the last element in \bold{coll}.
#'
#' @section Corner Cases:
#'    Throws an error if \bold{coll} has less than one element; this is
#'    because any other corner case would violate the function's type-signature.
#'
#' @family selection_functions
#'
#' @template
#'    Variadic
#'
#' @example
#'    inst/examples/example-xLastOf.R
#'
#' @rdname xLastOf
#' @export

xLastOf <- MakeFun	(function (coll) {


	MACRO( Must $ Be_Longer_Than(0, coll) )

	coll[[ length(coll) ]]
})

#' @rdname xLastOf
#' @export

xLastOf_ <- MakeVariadic(xLastOf, 'coll')
