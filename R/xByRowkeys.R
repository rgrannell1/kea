
#' $ xByRownames
#'
#' Convert a matrix or data.frame to a collection of column names.
#'
#' @details
#'     \bold{xByRownames} returns the column names of a data frame or matrix.
#'
#' @usage
#'      x_(  ) $ xByRownames()
#'
#' @return
#'      An arrow containing a character vector.
#'
#' @family methods
#'
#' @name xByRownames

xByRowkeys <- function (colls) {
	rownames(colls)
}
