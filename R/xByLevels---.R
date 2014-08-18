
#' xByLevels
#'
#' Convert a factor to a collection of levels.
#'
#' @param
#'     fact a factor. The factor to get the levels of.
#'
#' @return
#'     A kea object containing a character vector.
#'
#' @section Corner Cases:
#'     Returns character(0) for empty factors.
#'
#' @family methods
#'
#' @template S-Uncertain
#' @name xByLevels

xByLevels <- MakeFun('xByLevels', function (fact) {

	fact_levels <- levels(fact)

	if (length(fact_levels) == 0) {
		character(0)
	} else {
		fact_levels
	}
})
