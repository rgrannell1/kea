
#' xRepeat
#' 
#' Repeat a collection several times.
#'
#' @param a nonnegative positive number
#' @param coll a collection
#'
#' @return a list.
#'
#' @section Corner Cases: 
#'     returns the empty list if \code{coll} is length-zero or num is zero.
#' @template glossary
#'
#' @examples inst/examples/blank.R
#' @export

xRepeat <- function (num, coll) {
	# number -> Collection any -> [any]

	pcall <- sys.call()

	assert(
		!missing(num), pcall,
		exclaim$parameter_missing(num))

	assert(
		!missing(coll), pcall, 
		exclaim$parameter_missing(coll))

	assert(
		is.numeric(num), pcall,
		exclaim$must_be_numeric(num))

	assert(
		length(num) == 1, pcall,
		exclaim$must_have_length(num, 1))

	assert(
		num >= 0, pcall,
		exclaim$must_be_grequal_than(num, 0))

	assert(
		round(num) == num, pcall,
		exclaim$must_be_whole(num))

	if (num == 0) {
		list()
	} else {
		rep(as.list(coll), num)		
	}
}
