
#' xCycle
#'
#' Generate a cyclic permutation of a collection.
#'
#' @section Type Signature:
#'     <number> -> |any| -> [any]
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
#'     \code{xCycle_(0, 1:4)}
#'
#'     \code{xCycle_(1, 1:4)}
#'
#'     \code{xCycle_(2, 1:4)}
#'
#'     \code{xCycle_(3, 1:4)}
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
#'    If \bold{coll} or \bold{num} is empty the empty list is returned.
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

xCycle <- MakeFun('xCycle', function (num, coll) {

	MACRO( Must_Be_Whole(num) )
	MACRO( Must_Be_Finite(num) )

	cCycle(num, coll)

})

#' @rdname xCycle
#' @export

xCycle_ <- MakeVariadic(xCycle, 'coll')
