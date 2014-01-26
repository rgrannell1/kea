
#' xCycle
#'
#' Generate a cyclic permutation of a collection.
#'
#' @param
#'      num a whole number. The magnitude gives number of elements
#'      to cycle, and the sign gives the direction: positive numbers cause
#'      elements from the tail of \bold{coll} to be prepended to the output,
#'      while negative numbers cause elements from the head of \bold{coll} to
#'      appended to the output.
#'
#' @param
#'      coll a collection. The collection to cycle.
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
#'    inst/examples/example-xCycle.R
#'
#' @rdname xCycle
#' @export

xCycle <- function (num, coll) {
	# number -> Collection any -> Collection any
	# get a cyclic permutation of a collection.

	invoking_call <- sys.call()

	assert(
		!missing(num), invoking_call,
		exclaim$parametre_missing(num))

	assert(
		!missing(coll), invoking_call,
		exclaim$parametre_missing(coll))

	insist$must_be_collection(num, invoking_call)
	num <- to_value_unit(as_typed_vector(num, 'numeric'))

	insist$must_be_of_length(num, 1)
	insist$must_be_whole(num, invoking_call)
	insist$must_be_collection(coll, invoking_call)

	if (length(coll) == 0) {
		list()
	} else {
		indices <- ((seq_along(coll) - 1 + num) %% length(coll)) + 1
		as.list(coll)[indices]
	}
}

#' @rdname xCycle
#' @export

xCycle... <- function (num, ...) {
	xCycle(num, list(...))
}
