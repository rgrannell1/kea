
#' $ xByLevels
#'
#' Convert a collor to a collection of levels.
#'
#' @usage
#'      x_(  ) $ xByLevels()
#'
#' @return
#'      An kiwi object containing a character vector.
#'
#' @family methods
#'
#' @name xByLevels

xByLevels <- MakeFun(function (coll) {

	MACRO( Must $ Not_Be_Missing(fn) )
	MACRO( Must $ Not_Be_Missing(coll) )

	coll_levels <- levels(coll)

	if (length(coll_levels) == 0) {
		character(0)
	} else {
		coll_levels
	}
})
