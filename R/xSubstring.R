
#' xSubstring
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
#'    inst/examples/example-xSubstring.R
#'
#' @rdname xSubstring
#' @export

xSubstring <- MakeFun(function (str, nums) {
	# str -> integer -> Vector str
	# subset a str using normal R vector indexing.

	invoking_call <- sys.call()

	MACRO( arrow ::: Must $ Not_Be_Missing(str) )
	MACRO( arrow ::: Must $ Not_Be_Missing(nums) )

	MACRO( arrow ::: Must $ Be_Collection(str) )
	MACRO( arrow ::: Must $ Be_Collection(nums) )

	str <- unit_to_value(as_atom(str, "character"))
	nums <- as_typed_vector(nums, "numeric")

	insist $ must_be_whole(nums, invoking_call)

	if (length(str) == 0 || (length(nums) == 1 && nums == 0)) {
		character(0)
	} else if (length(nums) == 0) {
		str
	} else {

		assert(
			max(nums) <= nchar(str), invoking_call, "")

		chars <- strsplit(str, "")[[1]]
		chars <- chars[nchar(chars) > 0]

		paste0(chars[nums], collapse = "")
	}
})

#' @rdname xSubstring
#' @export

xSubstring... <- function (str, ...) {
	xSubstring(str, list(...))
}
