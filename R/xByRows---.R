
#' xByRows
#'
#' Convert a matrix or data.frame to a collection of rows.
#'
#' @details
#'     \bold{xByRows} is the most common way in which data-frames
#'     are reformatted for use by kiwi. Internally dataframes are
#'     represented as lists of lists, and kiwi prefers this explicit representation.
#'
#'     Column names are preserved by \bold{xByRows}.
#'
#' @usage
#'      x_(  ) $ xByRows()
#'
#' @param
#'     tab a matrix or data frame. The object to convert to column-lists.
#'
#' @return
#'      A kiwi object containing a list of lists.
#'
#' @family methods
#'
#' @name xByRows

xByRows <- MakeFun(function (tab) {

	dims <- dim(tab)

	if (dims[1] == 0 && dims[2] == 0) {
		# -- if both are empty, return list()
		list()
	} else if (dims[1] == 0) {
		# -- no rows.
		list()
	} else if (dims[2] == 0) {
		# -- no columns.
		replicate(max(dims), list())
	} else {
		apply(tab, 1, as.list)
	}
})
