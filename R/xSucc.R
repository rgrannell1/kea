
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
#' @example inst/examples/blank.R
#' @export

xSucc <- function (nums) {
	# Collection number -> Vector number
	# returns the successor of a vector of nums.

	pcall <- sys.call()

	assert(
		!missing(nums), pcall,
		exclaim$parameter_missing(nums))

	assert(
		is_collection(nums), pcall,
		exclaim$must_be_collection(nums))
	
	nums <- coerce_to_vector(nums, 'numeric')

	if (length(nums) == 0) {
		nums
	} else {
		nums + 1		
	}
}
