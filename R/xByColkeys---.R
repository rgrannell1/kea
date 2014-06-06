
#' xByColkeys
#'
#' Convert a matrix or data.frame to a collection of column names.
#'
#' @details
#'     \bold{xByColkeys} returns the column names of a data frame or matrix.
#'
#' @usage
#'      x_(  ) $ xByColkeys()
#'
#' @return
#'      A kiwi containing a character vector.
#'
#' @family methods
#'
#' @name xByColkeys

xByColkeys <- MakeFun(function (colls) {

	MACRO( Must $ Not_Be_Missing(colls) )

	colnames(colls)
})
