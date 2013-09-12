
#' xSucc
#' 
#' Decrement a collection of numbers.
#'
#' @param nums a collection of numbers.
#'
#' @return a vector of numbers.
#'
#' @section Corner Cases: 
#'     If \code{nums} is empty then the unit of that vector is returned.
#' @template glossary
#'
#' @examples 
#' @export

xSucc <- function (nums) {
	# Collection number -> Vector number
	# returns the successor of a vector of nums.

	pcall <- sys.call()

	require_a(traits$collection_of_number, nums, pcall)
	nums <- unlist(nums)

	if (length(nums) == 0) {
		nums
	} else {
		nums + 1		
	}
}
