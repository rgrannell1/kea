
#' xDrop
#'
#' Take several elements from the head of a collection.
#'
#' @param
#'      num a nonnegative whole number. The number of elements to
#'      return from the input collection.
#'
#' @param
#'      coll a collection. The collection to subset.
#'
#' @param
#'    ... see above.
#'
#' @return
#'    A list.
#'
#' @section Corner Cases:
#'    If \bold{coll} is empty the empty list is returned.
#'
#' @family selection_functions
#'
#' @template
#'    Variadic
#'
#' @example
#'    inst/examples/example-xDrop.R
#'
#' @rdname xDrop
#' @export

xDrop <- function (num, coll) {
	# Collection any -> [any]
	# take the first num values of collection.

	invoking_call <- sys.call()

	insist $ must_not_be_missing(num)
	insist $ must_not_be_missing(coll)

	num <- unit_to_value(as_atom(num, 'numeric'))

	insist $ must_be_whole(num, invoking_call)
	insist $ must_be_grequal_than(num, 0, invoking_call)

	insist $ must_be_collection(coll, invoking_call)

	if (length(coll) == 0 || num >= length(coll)) {
	 	list()
	} else {
		as.list(coll)[(num + 1) : length(coll)]
	}
}

#' @rdname xDrop
#' @export

xDrop... <- function (num, ...) {
	xDrop(num, list(...))
}
