
#' $ xByValues
#'
#' Convert a factor to a collection of the factor's data.
#'
#' @usage
#'      x_(  ) $ xByLevels()
#'
#' @return
#'      An kiwi object containing a list.
#'
#' @family methods
#'
#' @name xByLevels

xByValues <- MakeFun(function (coll) {

	MACRO( Must $ Not_Be_Missing(coll) )

	values <- as.vector(coll)

	if (length(values) == 0) {
		character(0)
	} else {
		values
	}
})