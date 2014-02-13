
#' xCycle
#'
#' Generate a cyclic permutation of a collection.
#'
#' @details
#'
#'    Cyclic permutations are an important combinatorial object.
#'    There are four cyclic permutations of a length-four collection:
#'
#'     \code{[1, 2, 3, 4]}
#'
#'     \code{[2, 3, 4, 1]}
#'
#'     \code{[3, 4, 1, 2]}
#'
#'     \code{[4, 1, 2, 3]}
#'
#'     Which map to
#'
#'     \code{xCycle(0, 1:4)}
#'
#'     \code{xCycle(1, 1:4)}
#'
#'     \code{xCycle(2, 1:4)}
#'
#'     \code{xCycle(3, 1:4)}
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
#' @family combinatoric_functions
#'
#' @family set_functions
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

	insist $ must_not_be_missing(num)

	insist $ must_not_be_missing(coll)

	insist $ must_be_collection(num, invoking_call)
	num <- unit_to_value(as_typed_vector(num, 'numeric'))

	insist $ must_be_of_length(num, 1)
	insist $ must_be_whole(num, invoking_call)
	insist $ must_be_collection(coll, invoking_call)

	if (length(coll) == 0) {
		list()
	} else if (length(coll) == 1) {
		as.list(coll)
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
