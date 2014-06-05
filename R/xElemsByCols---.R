
#' $ xElemsByCols
#'
#' Enumerate the elements of a matrix down columns.
#'
#' @usage
#'      x_(  ) $ xElemsByCols()
#'
#' @param
#'      colls a matrix.
#'
#' @return
#'      An arrow object containing a list.
#'
#' @family methods
#'
#' @name xElemsByCols

xElemsByCols <- MakeFun(function (colls) {

	MACRO( Must $ Not_Be_Missing(colls) )

	if (prod(dim(colls) == 0)) {
		list()
	} else {
		as.list(colls)
	}
})
