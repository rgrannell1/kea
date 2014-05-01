#' xAt
#'
#'
#' @section Type Signature:
#'    |numeric| -> |any| -> any
#'
#' Select the value at an index in a collection.
#'
#' @details
#'     \bold{xAt} is similar to base R's subsetting operator '[[',
#'     except that it performs more validation on the input indices,
#'     and it acts as a normal function.
#'
#' @param
#'    num a whole number. The index to select the collection at.
#'    in \bold{coll}.
#'
#' @param
#'    coll a collection. The collection to subset.
#'
#' @param
#'    ... see above.
#'
#' @return
#'    A list.
#'
#' @section Corner Cases:
#'    \bold{xAt} does not allow subscripting of values out of bounds.
#'    If an index larger than the maximum value in \bold{coll} is given
#'    an error is thrown.
#'
#' @template
#'    Variadic
#'
#' @example
#'    inst/examples/example-xAt.R
#'
#' @family selection_functions
#'
#' @rdname xAt
#' @export

xAt <- MakeFun(function (num, coll) {

	MACRO( Must $ Not_Be_Missing(num) )
	MACRO( Must $ Not_Be_Missing(coll) )

	MACRO( Must $ Be_Collection(num) )
	MACRO( Must $ Be_Collection(coll) )

	num <- unit_to_value(as_atom(num, 'numeric'))

	MACRO( Must $ Be_Whole(num) )
	MACRO( Must $ Be_Positive_Indices(num, coll) )

	coll[[num]]
})

#' @rdname xAt
#' @export

xAt_ <- MakeVariadic(xAt, 'coll')
