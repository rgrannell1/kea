
#' xByLevels
#'
#' Convert a collor to a collection of levels.
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

xByLevels <- MakeFun(function (coll) {



	coll_levels <- levels(coll)

	if (length(coll_levels) == 0) {
		character(0)
	} else {
		coll_levels
	}
})
