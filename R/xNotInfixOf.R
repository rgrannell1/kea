
#' xNotInfixOf
#'
#' Is a collection not an unbroken subsequence of another?
#'
#' @section Type Signature:
#'     |any| -> |any| -> &lt;logical>
#'
#' @param
#'    coll1 a collection. Then unbroken subsequence to test for absense of.
#'
#' @param
#'    coll2 a collection. The collection to test for a subsequence match.
#'
#' @param
#'    ... see above.
#'
#' @return
#'    A boolean value.
#'
#' @section Corner Cases:
#'    If the first collection is longer than the second, True is returned.
#'    If either collection is length-zero, \bold{logical(0)} is returned.
#'
#' @template
#'    Variadic
#'
#' @example
#'    inst/examples/example-xNotInfixOf.R
#'
#' @family value_testing_functions
#'
#' @rdname xNotInfixOf
#' @export

xNotInfixOf <- MakeFun('xNotInfixOf', function (coll1, coll2) {

	if (length(coll1) == 0 || length(coll2) == 0) {
		logical(0)
	} else {

		# -- cannot be an infix.
		if (length(coll1) > length(coll2)) {
			return(True)
		}

		for ( ith in 1:(length(coll2) - length(coll1)) ) {

			all_true    <- True
			subsequence <- coll2[ith:(ith + length(coll1) - 1)]

			for (jth in seq_along(coll1)) {
				if (!identical( coll1[[jth]], subsequence[[jth]] )) {
					all_true <- False
				}
			}

			if (all_true) {
				return (False)
			}
		}

		True
	}

})

#' @rdname xNotInfixOf
#' @export

xNotInfixOf_ <- MakeVariadic(xNotInfixOf, 'coll2')
