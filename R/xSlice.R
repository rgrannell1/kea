
#' xSlice
#'
#' Select a collection at certain indices.
#'
#' @section Type Signature:
#'     |numeric| -> |any| -> [any]
#'
#' @details
#'     \bold{xSlice} is similar to base R's subsetting operator '[',
#'     except that it performs more validation on the input indices,
#'     and it acts as a normal function.
#'
#' @param
#'    nums a vector of whole numbers. Indices to select values
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
#'    \bold{xSlice} does not allow subscripting values out of bounds, unlike
#'    base R. If an index larger than the maximum value in \bold{coll} is given
#'    an error is thrown.
#'
#' @template
#'    Variadic
#'
#' @example
#'    inst/examples/example-xSlice.R
#'
#' @family selection_functions
#'
#' @rdname xSlice
#' @export

xSlice <- MakeFun(function (nums, coll) {

	MACRO( Fix(xSlice, nums, coll) )

	MACRO( Must $ Be_Collection(nums) )
	MACRO( Must $ Be_Collection(coll) )

	nums <- unit_to_value(as_typed_vector(nums, 'numeric'))

	MACRO( Must $ Be_Whole(nums) )
	MACRO( Must $ Be_Indices(nums, coll) )

	as.list(coll[nums])
})

#' @rdname xSlice
#' @export

xSlice_ <- MakeVariadic(xSlice, 'coll')
