
#' xElemsByRows
#'
#' Enumerate the elements of a matrix across rows.
#'
#' @usage
#'      x_(  ) $ xElemsByRows()
#'
#' @param
#'      tab a matrix. The matrix to enumerate by elements.
#'
#' @return
#'      A kiwi object containing a list.
#'
#' @section Corner Cases:
#'
#' @family methods
#'
#' @name xElemsByRows

xElemsByRows <- MakeFun(function (tab) {

	if (prod(dim(tab) == 0)) {
		list()
	} else {
		as.list(t(tab))
	}
})
