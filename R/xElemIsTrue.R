
#' xElemIsTrue
#'
#' Is an element of a collection true?
#'
#' @param
#'    coll a collection. The collection to test each element
#'    of for being true.
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
#'    inst/examples/example-xElemIsTrue.R
#'
#' @family value_testing_functions
#'
#' @rdname xElemIsTrue
#' @export

xElemIsTrue <- MakeFun(function (coll) {

	MACRO( Must $ Not_Be_Missing(coll) )
	MACRO( Must $ Be_Collection(coll) )

	vapply(coll, isTRUE, logical(1))
})

#' @rdname xElemIsTrue
#' @export

xElemIsTrue_ <- MakeFun(function (...) {

	MACRO( Must $ Have_Canonical_Arguments() )

	xElemIsTrue(list(...))
})
