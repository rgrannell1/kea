
#' xPoll
#'
#' Count the number of times a function returns
#' true when mapped over a collection.
#'
#' @section Type Signature:
#'     (any -> <logical>) -> |any| -> <integer>
#'
#' @param
#'    pred a unary predicate function. The function with
#'    which to poll each element of the input collection.
#'
#' @param
#'    coll a collection. The collection to poll.
#'
#' @param
#'    ... see above.
#'
#' @return
#'    A non-negative whole number.
#'
#' @section Corner Cases:
#'    Returns integer(0) when given an empty collection, and zero
#'    if no matches are found.
#'
#' @template
#'    Variadic
#'
#' @example
#'    inst/examples/example-xPoll.R
#'
#' @family quantifier_functions
#' @family math_functions
#'
#' @rdname xPoll
#' @export

xPoll <- MakeFun(function (pred, coll) {

	# C++ is currently much slower!

	MACRO( Must_Have_Arity(pred, 1) )

	if (length(coll) == 0)
		integer(0)
	else

		MACRO( Try_Higher_Order_Function({

			count <- 0

			for (ith in seq_along(coll)) {

				is_match <- pred( coll[[ith]] )

				MACRO( Must_Be_Flag(is_match, pred) )

				if (identical(is_match, TRUE)) {
					count <- count + 1
				}
			}

			count

		}) )

	#cPoll(pred, coll)

})

#' @rdname xPoll
#' @export

xPoll_ <- MakeVariadic(xPoll, 'coll')
