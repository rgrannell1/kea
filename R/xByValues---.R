
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

xByValues <- function (fact) {

	MACRO( Must $ Not_Be_Missing(fact) )

	values <- as.vector(fact)

	if (length(values) == 0) {
		character(0)
	} else {
		values
	}
}