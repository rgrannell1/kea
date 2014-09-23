
#' xCarve
#'
#' Subset a string using normal R vector indexing.
#'
#' @section Type Signature:
#'     |numeric| -> |character| -> &lt;character>
#'
#' @param
#'    str a string. The string to subset.
#'
#' @param
#'    nums a collection of whole numbers. The indices to select
#'    characters with. Negative, positive and zero indices are allowed,
#'    but normal R index semantics dictate how they can intermix.
#'
#' @param
#'    ... see above.
#'
#' @return
#'      A character vector.
#'
#' @section Corner Cases:
#'      Returns the empty list if \bold{coll} or \bold{str} is length-zero. If any number in
#'      nums is larger than the length of \bold{coll} an error is thrown.
#'
#' @family text_processing_functions
#'
#' @example
#'    inst/examples/example-xCarve.R
#'
#' @rdname xCarve
#' @export

xCarve <- MakeFun('xCarve', function (nums, str) {

	# -- handles Na and NaN
	MACRO( Must_All_Be_Whole(nums) )

	if (length(str) == 0 || (length(nums) == 1 && nums == 0)) {
		character(0)
	} else if (length(nums) == 0) {
		character(0)
	} else {

		chars <- str_split("", str)

		MACRO( Must_All_Be_Indices(nums, chars) )

		out        <- paste0(chars[nums], collapse = "")
		names(out) <- names(str)
		out
	}
})
