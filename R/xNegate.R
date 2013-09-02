
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

#| function: xNegate version: 0.1 finished: false 

xNegate <- function (nums) {
	# Collection number -> number
	# flips the sign of each number.

	pcall <- sys.call()

	require_a("collection_of_length_one", nums, pcall)
	nums <- unlist(nums)

	require_a(c('double', 'integer'), nums, pcall)

	if (length(nums) == 0) {
		nums
	} else {
		-nums
	}
}


