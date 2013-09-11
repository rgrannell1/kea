
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
#' @examples 
#' @export

xNegate <- function (nums) {
	# Collection number -> number
	# flips the sign of each number.

	pcall <- sys.call()

	require_a(c(
		"collection_of_double", 
		"collection_of_integer"), nums, pcall)

	nums <- unlist(nums)

	if (length(nums) == 0) {
		nums
	} else {
		-nums
	}
}


