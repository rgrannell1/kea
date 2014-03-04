
#' xSlice
#'
#' Select a collection at certain indices.
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
	# Vector numbers -> Collection any -> Collection any
	# select elements of a collection using indices.

	invoking_call <- sys.call()

	MACRO( arrow ::: Must $ Not_Be_Missing(nums) )
	MACRO( arrow ::: Must $ Not_Be_Missing(coll) )

	MACRO( arrow ::: Must $ Be_Collection(nums) )
	MACRO( arrow ::: Must $ Be_Collection(coll) )

	nums <- unit_to_value(as_typed_vector(nums, 'numeric'))

	insist $ must_be_whole(nums, invoking_call)
	insist $ must_be_indices_of(nums, coll, invoking_call)

	as.list(coll[nums])
})

#' @rdname xSlice
#' @export

xSlice... <- function (nums, ...) {
	xSlice(nums, list(...))
}
