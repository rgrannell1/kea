
#' xByColkeys
#'
#' Convert a matrix or data.frame to a collection of column names.
#'
#' @details
#'     \bold{xByColkeys} returns the column names of a data frame or matrix.
#'
#' @param
#'      tab a matrix or data frame. The tabular structure to get the column names of.
#'
#' @return
#'      A kea containing a character vector.
#'
#' @section Corner Cases:
#'     Returns character(0) when \bold{tab} is length-zero or has no keys.
#'
#' @family methods
#'
#' @template S-Uncertain
#' @name xByColkeys

xByColkeys <- MakeFun('xByColkeys', function (tab) {
	colnames(tab)
})
