
#' xElemsByRows
#'
#' Enumerate the elements of a matrix across rows.
#'
#' @usage
#'      x_(  ) $ xElemsByRows()
#'
#' @return
#'      A kiwi object containing a list.
#'
#' @family methods
#'
#' @name xElemsByRows

xElemsByRows <- MakeFun(function (colls) {

	MACRO( Must $ Not_Be_Missing(colls) )

	if (prod(dim(colls) == 0)) {
		list()
	} else {
		as.list(t(colls))
	}
})
