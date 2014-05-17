
#' $ xByRows
#'
#' Convert a matrix or data.frame to a collection of rows.
#'
#' @details
#'     \bold{xByRows} is the most common way in which data-frames
#'     are reformatted for use by arrow. Internally dataframes are
#'     represented as lists of lists, and arrow prefers this explicit representation.
#'
#'     Column names are preserved by \bold{xByRows}.
#'
#'
#' @usage
#'      x_(  ) $ xByRows()
#'
#' @return
#'      An arrow object containing a list of lists.
#'
#' @family methods
#'
#' @name xByRows

NULL
