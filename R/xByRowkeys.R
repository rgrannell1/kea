
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
#' @return
#'      A kiwi containing a character vector.
#'
#' @family methods
#'
#' @name xByRowkeys

xByRowkeys <- MakeFun(function (colls) {

	rownames(colls)
})
