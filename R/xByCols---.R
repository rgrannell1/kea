
#' xByCols
#'
#' Convert a matrix or data.frame to a collection of columns.
#'
#' @param
#'     tab a matrix or data frame. The object to convert to column-lists.
#'
#' @return
#'      A kea object containing a list of lists.
#'
#' @section Corner Cases:
#'     Returns the empty list if \bold{tab} has no columns or have zero x zero dimensions.
#'
#' @family methods
#'
#' @name xByCols

xByCols <- MakeFun('xByCols', function (tab) {

	dims <- dim(tab)

	if (dims[1] == 0 && dims[2] == 0) {
		# -- if both are empty, return list()
		list()
	} else if (dims[2] == 0) {
		# -- no columns
		list()
	} else if (dims[1] == 0) {
		# -- no rows
		replicate(max(dims), list())
	} else {
		apply(tab, 2, as.list)
	}
})
