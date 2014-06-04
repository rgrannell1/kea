
#' $ xByLevels
#'
#' Convert a factor to a collection of levels.
#'
#' @usage
#'      x_(  ) $ xByLevels()
#'
#' @return
#'      An arrow object containing a character vector.
#'
#' @family methods
#'
#' @name xByLevels

xByLevels <- function (fact) {

	MACRO( Must $ Not_Be_Missing(fn) )
	MACRO( Must $ Not_Be_Missing(fact) )

	fact_levels <- levels(fact)

	if (length(fact_levels) == 0) {
		character(0)
	} else {
		fact_levels
	}
}
