
#' xNotNa
#' 
#' Is an element of a collection not na?
#'
#' @param coll a collection.
#'
#' @return a vector of boolean values.
#'
#' @section Corner Cases: 
#'     returns logical(0) if \code{coll} is length-zero.
#' @template glossary
#'
#' @example inst/examples/blank.R
#' @export

xNotNa <- function (coll) {
	# Collection a -> Vector boolean
	# Is an element of a collection not na?

	pcall <- sys.call()

	assert(
		!missing(coll), pcall, 
		exclaim$parameter_missing(coll))

	coll <- dearrowise(coll)

	assert(
		is_collection(coll), pcall,
		exclaim$must_be_collection(coll))

	if (length(coll) == 0) {
		logical(0)
	} else {
		vapply(coll, function (x) {
			!identical(x, NA) &&
			!identical(x, NA_integer_) &&
			!identical(x, NA_real_) &&
			!identical(x, NA_character_) &&
			!identical(x, NA_complex_)
		}, logical(1), USE.NAMES = False)		
	}
}
