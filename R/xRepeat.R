
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
#' @examples 
#' @export

xRepeat <- function (num, coll) {
	# number -> Collection any -> [any]

	pcall <- sys.call()

	assert(length(num) == 1, pcall) 
	assert(is.numeric(num) && num >= 0, pcall)
	assert(round(num) == num, pcall)

	if (num == 0) {
		list()
	} else {
		rep(as.list(coll), num)		
	}
}
