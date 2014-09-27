
#' xByValues
#'
#' Convert a factor to a collection of the factor's data.
#'
#' @param
#'     fact a factor. The factor to get the underlying values from.
#'
#' @return
#'      A kea object containing a list.
#'
#' @section Corner Cases:
#'
#' @family methods
#'
#' @template S-Uncertain
#' @name xByValues

xByValues <- MakeFun(function (fact) {

	values <- as.vector(fact)

	if (length(values) == 0) {
		character(0)
	} else {
		values
	}
})
