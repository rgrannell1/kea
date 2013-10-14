
#' xIsNan
#' 
#' Is an element in a collection NaN?
#'
#' @param coll a collection
#'
#' @return a list.
#'
#' @section Corner Cases: 
#'     returns True if coll is length-zero.
#'
#' @template glossary
#'
#' @example inst/examples/blank.Rinst/examples/blank.R
#' @export

xIsNan <- function (coll) {
	# collection any -> vector Boolean

	pcall <- sys.call()

	assert(
		!missing(coll), pcall, 
		exclaim$parameter_missing(coll))

	coll <- dearrowise(coll)

	assert(
		is_collection(coll), pcall,
		exclaim$must_be_collection(coll))

	if (length(coll) == 0 || is.nan(coll)) {
		# empty pairlist.
		True
	} else {
		res <- vector(mode = 'logical', length(coll))

		for (ith in seq_along(coll)) {
			res[ith] <- identical(coll[[ith]], NaN)
		}
		res		
	}
}

