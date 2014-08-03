
#' xElemsByRows
#'
#' Enumerate the elements of a matrix across rows.
#'
#' @param
#'      tab a matrix. The matrix to enumerate by elements.
#'
#' @return
#'      A kea object containing a list.
#'
#' @section Corner Cases:
#'      Returns the empty list if either dimension of \bold{tab} is empty.
#'
#' @family methods
#'
#' @name xElemsByRows

xElemsByRows <- MakeFun('xElemsByRows', function (tab) {

	if (prod(dim(tab) == 0)) {
		list()
	} else {
		as.list(t(tab))
	}
})
