
#' xAtCol
#'
#' Select a column of values from a collection of rows.
#'
#' @param
#'     num a whole number. The number of the column to select.
#'
#' @param
#'     colls a collection of collections. The collection of
#'     rows to select a column from.
#'
#' @param
#'    ... see above.
#'
#' @return
#'    A list.
#'
#' @template
#'    Variadic
#'
#' @example
#'    inst/examples/example-xAtCol.R
#'
#' @family selection_functions
#'
#' @rdname xAtCol
#' @export

xAtCol <- MakeFun(function (num, colls) {
	# number -> Collection Collection any -> Collection any
	# select a column of numbers.

	invoking_call <- sys.call()

	MACRO( arrow ::: Must $ Not_Be_Missing(num) )
	MACRO( arrow ::: Must $ Not_Be_Missing(colls) )

	MACRO( arrow ::: Must $ Be_Collection(num) )
	MACRO( arrow ::: Must $ Be_Collection(colls) )

	if (length(colls) == 0) {
		list()
	} else {

		MACRO( arrow ::: Must $ Be_Collection_Of_Collections(colls) )
		insist $ must_be_collections_of_length_grequal_than(
			colls, num, invoking_call  )

		lapply(colls, function (coll) {
			coll[[num]]
		})
	}
})

#' @rdname xAtCol
#' @export

xAtCol... <- function (num, ...) {
	xAtCol(num, list(...))
}
