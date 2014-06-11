
#' xByValues
#'
#' Convert a factor to a collection of the factor's data.
#'
#' @usage
#'      x_(  ) $ xByLevels()
#'
#' @return
#'      A kiwi object containing a list.
#'
#' @family methods
#'
#' @name xByLevels

xByValues <- MakeFun(function (coll) {



	values <- as.vector(coll)

	if (length(values) == 0) {
		character(0)
	} else {
		values
	}
})