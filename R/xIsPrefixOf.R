
#' xIsPrefixOf
#'
#' Is a collection a prefix of another?
#'
#' @section Type Signature:
#'     |any| -> |any| -> &lt;logical>
#'
#' @param
#'    coll1 a collection. The prefix to test for.
#'
#' @param
#'    coll2 a collection. The collection to test for a prefix.
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
#'    inst/examples/example-xIsPrefixOf.R
#'
#' @family value_testing_functions
#'
#' @rdname xIsPrefixOf
#' @export

xIsPrefixOf <- MakeFun('xIsPrefixOf', function (coll1, coll2) {

	if (length(coll1) == 0 || length(coll2) == 0) {
		logical(0)
	} else {

		# -- cannot be a prefix.
		if (length(coll1) > length(coll2)) {
			return(False)
		}

		for ( ith in seq_len(length(coll1)) ) {

			if (!identical( coll1[[ith]], coll2[[ith]] )) {
				return(False)
			}

		}

		return(True)
	}

})

#' @rdname xIsPrefixOf
#' @export

xIsPrefixOf_ <- MakeVariadic(xIsPrefixOf, 'coll2')
