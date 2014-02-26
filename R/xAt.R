
#' xAt
#'
#' Select a particular value from a collection.
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
#' @rdname xAt
#' @export

xAt <- function (num, coll) {

	invoking_call <- sys.call()

	insist $ must_not_be_missing(coll)
	insist $ must_not_be_missing(num)

	insist $ must_be_collection(coll, invoking_call)
	insist $ must_be_collection(num, invoking_call)

	num <- unit_to_value(as_atom(num, 'numeric'))

	insist $ must_be_whole(num, invoking_call)
	insist $ max_must_be_less_than_length_of(num, coll, invoking_call)
	insist $ must_be_greater_than(num, 0, invoking_call)

	coll[[num]]
}

#' @rdname xAt
#' @export

xAt... <- function (num, ...) {
	xAt(num, list(...))
}
