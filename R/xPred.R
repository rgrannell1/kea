
#' xPred
#' 
#' Increment a vector of numbers.
#'
#' @param nums a vector of non-complex numbers.
#'
#' @return a vector of non-complex numbers.
#'
#' @section Corner Cases: 
#'     returns the unit of the vector if \code{nums} is length-zero.
#'     
#' @template glossary
#'
#' @example inst/examples/blank.R
#' @export

xPred <- function (nums) {
	# Collection number -> Vector number
	# returns the predeccesor of a vector of nums.

	pcall <- sys.call()

	assert(
		!missing(nums), pcall,
		exclaim$parameter_missing(nums))

	nums <- dearrowise(nums)

	assert(
		is_collection(nums), pcall,
		exclaim$must_be_collection(nums))
	
	nums <- coerce_to_typed_vector(nums, 'numeric')

	if (length(nums) == 0) {
		nums
	} else {
		nums - 1		
	}
}
