
#' Flip the sign of each number in a vector.
#' 
#' xNegate
#'
#' @param nums a vector of numbers.
#'
#' @return a vector of number.
#'
#' @section Corner Cases: 
#'     If nums is empty then the unit of the collection is returned.
#' @template glossary
#'
#' @examples inst/examples/blank.R
#' @export

xNegate <- function (nums) {
	# Collection number -> number
	# flips the sign of each number.

	pcall <- sys.call()

	assert(
		!missing(nums), pcall,
		exclaim$parameter_missing(nums))

	assert(
		is.vector(nums) || is.pairlist(nums), pcall,
		exclaim$must_be_collection(nums))
	
	assert(
		all(sapply(nums, length) == 1), pcall)
	
	assert(
		is.numeric(unlist(nums)), pcall, 
		exclaim$must_be_numeric( "unlist(nums)" ))

	nums <- unlist(nums)

	if (length(nums) == 0) {
		nums
	} else {
		-nums
	}
}


