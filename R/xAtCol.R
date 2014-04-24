
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

	MACRO( Must $ Not_Be_Missing(num) )
	MACRO( Must $ Not_Be_Missing(colls) )

	MACRO( Must $ Be_Collection(num) )
	MACRO( Must $ Be_Collection(colls) )

	if (length(colls) == 0) {
		list()
	} else {

		MACRO( Must $ Be_Collection_Of_Collections(colls) )
		MACRO( Must $ Be_Collection_Of_Lengths_In_Range(colls, num, Inf) )

		lapply(colls, function (coll) {
			coll[[num]]
		})
	}
})

#' @rdname xAtCol
#' @export

xAtCol_ <- function (num, ...) {

	MACRO( Must $ Have_Canonical_Arguments() )

	xAtCol(num, list(...))
}
