
#' xIsSuffixOf
#'
#' Is a collection a suffix of another?
#'
#' @section Type Signature:
#'     |any| -> |any| -> &lt;logical>
#'
#'
#' @param
#'    coll1 a collection. The suffix to test for.
#'
#' @param
#'    coll2 a collection. The collection to test for a suffix.
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
#'    inst/examples/example-xIsSuffixOf.R
#'
#' @family value_testing_functions
#'
#' @rdname xIsSuffixOf
#' @export

xIsSuffixOf <- MakeFun('xIsSuffixOf', function (coll1, coll2) {

	if (length(coll1) == 0 || length(coll2) == 0) {
		logical(0)
	} else {

		# -- cannot be a prefix.
		if (length(coll1) > length(coll2)) {
			return (False)
		}

		for (ith in seq_along(coll1)) {

			if (!identical(
				coll1[[ length(coll1) - ith + 1 ]],
				coll2[[ length(coll2) - ith + 1 ]] )) {

				return (False)
			}

		}

		return (True)
	}

})

#' @rdname xIsSuffixOf
#' @export

xIsSuffixOf_ <- MakeVariadic(xIsSuffixOf, 'coll2')
