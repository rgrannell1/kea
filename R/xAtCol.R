
#' xAtCol
#'
#' Select a column of values from a collection of rows.
#'
#' @section Type Signature:
#'    |numeric| -> ||any|| -> |any|
#'
#' @details
#'     If \bold{xAt} behaves like \bold{coll[[ith,]]} then
#'     \bold{xAtCol} is its counterpart, coll[[,ith]]. Tabular
#'     data can be encoded as a list of row-lists, and it is
#'     to be able to select this data "by-columns".
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
#' @section Corner Cases:
#'    If cols is length-zero the empty list is returned. \bold{xAtCol}
#'    can operate on lists of mixed-length collections, if the smallest
#'    collection has length equal to or longer than \bold{num}.s
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




	num <- unit_to_value(as_atom(num, 'numeric'))

	if (length(colls) == 0) {
		list()
	} else {

		MACRO( Must $ Be_Collection_Of_Lengths_In_Range(colls, num, Inf) )

		lapply(colls, function (coll) {
			coll[[num]]
		})
	}
})

#' @rdname xAtCol
#' @export

xAtCol_ <- MakeVariadic(xAtCol, 'colls')
