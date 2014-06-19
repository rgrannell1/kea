
#' xElemsByCols
#'
#' Enumerate the elements of a matrix down columns.
#'
#' @usage
#'      x_(  ) $ xElemsByCols()
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
#' @name xElemsByCols

xElemsByCols <- MakeFun(function (tab) {

	if (prod(dim(tab) == 0)) {
		list()
	} else {
		as.list(tab)
	}
})
