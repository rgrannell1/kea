
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
#' @param
#'      tab a matrix or data frame. The tabular structure to get the column names of.
#'
#' @return
#'      A kiwi containing a character vector.
#'
#' @family methods
#'
#' @name xByColkeys

xByColkeys <- MakeFun(function (tab) {
	colnames(tab)
})
