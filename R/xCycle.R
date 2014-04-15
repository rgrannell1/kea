
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
#'     \code{xCycle...(0, 1:4)}
#'
#'     \code{xCycle...(1, 1:4)}
#'
#'     \code{xCycle...(2, 1:4)}
#'
#'     \code{xCycle...(3, 1:4)}
#'
#' @param
#'      num a whole number. The magnitude gives number of elements
#'      to cycle, and the sign gives the direction: positive numbers cause
#'      elements from the tail of \bold{coll} to be prepended to the output,
#'      while negative numbers cause elements from the head of \bold{coll} to
#'      appended to the output.
#'
#' @param
#'      colls a collection of collections. The collection to cycle.
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
#' @template
#'    Variadic
#'
#' @example
#'    inst/examples/example-xCycle.R
#'
#' @rdname xCycle
#' @export

xCycle <- MakeFun(function (num, colls) {
	# number -> Collection any -> Collection any
	# get a cyclic permutation of a collection.

	MACRO( Must $ Not_Be_Missing(num) )
	MACRO( Must $ Not_Be_Missing(colls) )

	MACRO( Must $ Be_Collection(num) )
	MACRO( Must $ Be_Collection(colls) )

	num <- unit_to_value(as_atom(num, 'numeric'))

	MACRO( Must $ Be_Whole(num) )

	MACRO( Must $ Be_Collection_Of_Collections(colls) )
	MACRO( Must $ Be_Collection_Of_Equal_Length(colls) )

	if (length(colls) == 0) {
		list()
	} else {
		indices <- ((seq_along( colls[[1]])  - 1 + num) %% length( colls[[1]]) ) + 1

		lapply(colls, function (permutable) {
			as.list(permutable[indices])
		})
	}
})

#' @rdname xCycle
#' @export

xCycle... <- function (num, ...) {
	xCycle(num, list(...))
}
