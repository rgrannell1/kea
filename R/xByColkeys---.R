
#' $ xByColkeys
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
#'      An arrow containing a character vector.
#'
#' @family methods
#'
#' @name xByColkeys

xByColkeys <- function (colls) {
	colnames(colls)
}
