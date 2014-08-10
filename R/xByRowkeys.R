
#' xByRowkeys
#'
#' Convert a matrix or data.frame to a collection of column names.
#'
#' @details
#'     \bold{xByRowkeys} returns the column names of a data frame or matrix.
#'
#' @param
#'      tab a matrix or data frame. The tabular structure to get the row names of.
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
#' @name xByRowkeys

xByRowkeys <- MakeFun('xByRowkeys', function (tab) {
	rownames(tab)
})
