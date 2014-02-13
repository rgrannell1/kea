
#' xAt
#'
#' Select a collection at certain indices.
#'
#' @details
#'     \bold{xAt} is similar to base R's subsetting operator '[',
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
#'    \bold{xAt} does not allow subscripting values out of bounds, unlike
#'    base R. If an index larger than the maximum value in \bold{coll} is given
#'    an error is thrown.
#'
#' @template
#'    Variadic
#'
#' @example
#'    inst/examples/example-xAt.R
#'
#' @rdname xAt
#' @export

xAt <- function (nums, coll) {
	# Vector numbers -> Collection any -> Collection any
	# select elements of a collection using indices.

	invoking_call <- sys.call()

	insist $ must_not_be_missing(coll)
	insist $ must_not_be_missing(nums)

	insist $ must_be_collection(coll, invoking_call)
	insist $ must_be_collection(nums, invoking_call)

	nums <- unit_to_value(as_typed_vector(nums, 'numeric'))

	insist $ must_be_whole(nums, invoking_call)
	insist $ max_must_be_less_than_length_of(nums, coll, invoking_call)

	as.list(coll[nums])
}

#' @rdname xAt
#' @export

xAt... <- function (nums, ...) {
	xAt(nums, list(...))
}
