
#' xSliceString
#'
#' Subset a string using normal R vector indexing.
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
#'      Returns the empty list if \bold{coll} is length-zero. If any number in
#'      nums is larger than the length of \bold{coll} an error is thrown.
#'
#' @family text_processing_functions
#'
#' @template
#'    Variadic
#'
#' @example
#'    inst/examples/example-xSliceString.R
#'
#' @rdname xSliceString
#' @export

xSliceString <- MakeFun(function (str, nums) {
	# str -> integer -> Vector str
	# subset a str using normal R vector indexing.

	invoking_call <- sys.call()

	MACRO( Must $ Not_Be_Missing(str) )
	MACRO( Must $ Not_Be_Missing(nums) )

	MACRO( Must $ Be_Collection(str) )
	MACRO( Must $ Be_Collection(nums) )

	str <- unit_to_value(as_atom(str, "character"))
	nums <- as_typed_vector(nums, "numeric")

	MACRO( Must $ Be_Whole(nums) )

	if (length(str) == 0 || (length(nums) == 1 && nums == 0)) {
		character(0)
	} else if (length(nums) == 0) {
		str
	} else {


		chars <- strsplit(str, "")[[1]]
		chars <- chars[nchar(chars) > 0]

		MACRO( Must $ Be_Indices(nums, chars) )

		paste0(chars[nums], collapse = "")
	}
})

#' @rdname xSliceString
#' @export

xSliceString... <- function (str, ...) {
	xSliceString(str, list(...))
}
