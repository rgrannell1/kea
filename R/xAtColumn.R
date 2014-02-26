
#' xAtColumn
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
#' @rdname xAtColumn
#' @export

xAtColumn <- function (num, colls) {
	# number -> Collection Collection any -> Collection any
	# select a column of numbers.

	invoking_call <- sys.call()

	insist $ must_not_be_missing(nums)
	insist $ must_not_be_missing(colls)

	insist $ must_be_collection(colls, invoking_call)

	if (length(colls) == 0) {
		list()
	} else {

		insist $ must_be_collection_of_collections(colls, invoking_call)
		# min coll length must be larger than num

		lapply(colls, function (coll) {
			coll[[num]]
		})
	}
}

#' @rdname xAtColumn
#' @export

xAtColumn... <- function (num, ...) {
	xAtColumn(num, list(...))
}
