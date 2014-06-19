
#' xByValues
#'
#' Convert a factor to a collection of the factor's data.
#'
#' @usage
#'      x_(  ) $ xByLevels()
#'
#' @param
#'     fact a factor. The factor to get the underlying values from.
#'
#' @return
#'      A kiwi object containing a list.
#'
#' @section Corner Cases:
#'
#' @family methods
#'
#' @name xByLevels

xByValues <- MakeFun(function (fact) {

	values <- as.vector(fact)

	if (length(values) == 0) {
		character(0)
	} else {
		values
	}
})