
#' xIsInfixOf
#'
#' Is a collection an unbroken subsequence of another?
#'
#' @section Type Signature:
#'     |any| -> |any| -> &lt;logical>
#'
#' @param
#'    coll1 a collection. Then unbroken subsequence to test for.
#'
#' @param
#'    coll2 a collection. The collection to test for a subsequencematch.
#'
#' @param
#'    ... see above.
#'
#' @return
#'    A boolean value.
#'
#' @section Corner Cases:
#'    If the first collection is longer than the second, False is returned.
#'    If either collection is length-zero, \bold{logical(0)} is returned.
#'
#' @template
#'    Variadic
#'
#' @example
#'    inst/examples/example-xIsInfixOf.R
#'
#' @rdname xIsInfixOf
#' @export

xIsInfixOf <- MakeFun('xIsInfixOf', function (coll1, coll2) {

	if (length(coll1) == 0 || length(coll2) == 0) {
		logical(0)
	} else {

		# -- cannot be an infix.
		if (length(coll1) > length(coll2)) {
			return(False)
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
				return (True)
			}
		}

		False
	}

})

#' @rdname xIsInfixOf
#' @export

xIsInfixOf_ <- MakeVariadic(xIsInfixOf, 'coll2')
