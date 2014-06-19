
#' xByRowkeys
#'
#' Convert a matrix or data.frame to a collection of column names.
#'
#' @details
#'     \bold{xByRowkeys} returns the column names of a data frame or matrix.
#'
#' @usage
#'      x_(  ) $ xByRowkeys()
#'
#' @param
#'      tab a matrix or data frame. The tabular structure to get the rownames of.
#'
#' @return
#'      A kiwi containing a character vector.
#'
#' @section Corner Cases:
#'
#' @family methods
#'
#' @name xByRowkeys

xByRowkeys <- MakeFun(function (tab) {
	rownames(tab)
})
