
#' xByLevels
#'
#' Convert a factor to a collection of levels.
#'
#' @usage
#'      x_(  ) $ xByLevels()
#'
#' @return
#'      A kiwi object containing a character vector.
#'
#' @family methods
#'
#' @name xByLevels

xByLevels <- MakeFun(function (fact) {

	fact_levels <- levels(fact)

	if (length(fact_levels) == 0) {
		character(0)
	} else {
		fact_levels
	}
})
