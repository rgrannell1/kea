
#' xSubStr
#' 
#' Subset a string using normal R vector indexing.
#'
#' @param str a string.
#' @param nums indices of \code{str}.
#'
#' @return a character vector.
#'
#' @section Corner Cases: 
#'     returns the empty list if \code{coll} is length-zero.
#' @template glossary
#'
#' @examples inst/examples/blank.R
#' @export

xSubStr <- function (str, nums) {
	# str -> integer -> Vector str
	# subset a str using normal R vector indexing.
	
	pcall <- sys.call()

	assert(
		!missing(str), pcall,
		exclaim$parameter_missing(str))
	assert(
		!missing(nums), pcall)

	assert(
		is.character(str) && length(str) < 2, pcall)
	assert(
		is.numeric(nums) && all(round(nums) == nums), pcall)
	
	if (length(str) == 0) {
		character(0)
	} else if (length(nums) == 0) {
		str
	} else {
		assert(
			max(nums) <= nchar(str))

		chars <- strsplit(str, "")[[1]]
		paste0(chars[nums], collapse = "")
	}
}
