
#' $ xByValues
#'
#' Convert a factor to a collection of the factor's data.
#'
#' @usage
#'      x_(  ) $ xByLevels()
#'
#' @return
#'      An arrow object containing a list.
#'
#' @family methods
#'
#' @name xByLevels

xByValues <- function (coll) {

	MACRO( Must $ Not_Be_Missing(coll) )

	values <- as.vector(coll)

	if (length(values) == 0) {
		character(0)
	} else {
		values
	}
}