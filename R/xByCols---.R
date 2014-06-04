
#' $ xByCols
#'
#' Convert a matrix or data.frame to a collection of columns.
#'
#' @usage
#'      x_(  ) $ xByCols()
#'
#' @param
#'     colls a matrix or data frame. The object to convert to column-lists.
#'
#' @return
#'      An arrow object containing a list of lists.
#'
#' @family methods
#'
#' @name xByCols

xByCols <- function (colls) {

	MACRO( Must $ Not_Be_Missing(colls) )

	dims <- dim(colls)

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
		apply(colls, 2, as.list)
	}
}
