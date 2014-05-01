
#' xElemIsNa
#'
#' Is an element of a collection na?
#'
#' @param
#'    coll a collection. The collection to test each element
#'    of for being na.
#'
#' @param
#'    ... see above.
#'
#' @return
#'    A vector of boolean values.
#'
#' @template
#'    Variadic
#'
#' @example
#'    inst/examples/example-xElemIsNa.R
#'
#' @family value_testing_functions
#'
#' @rdname xElemIsNa
#' @export

xElemIsNa <- MakeFun(function (coll) {

	MACRO( Must $ Not_Be_Missing(coll) )
	MACRO( Must $ Be_Collection(coll) )

	vapply(coll, function (elem) {
		is.na(elem)
	}, logical(1))
})

#' @rdname xElemIsNa
#' @export

xElemIsNa_ <- MakeVariadic(xElemIsNa, 'coll')
