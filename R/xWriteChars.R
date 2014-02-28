
#' xWriteChars
#'
#' Export a collection of strings as a single line in a file.
#'
#' @param
#'    str a length-one character vector. The path to
#'    export to.
#'
#' @param
#'    strs a character vector. Lines to write to a file.
#'
#' @return
#'    null -
#'
#' @family text_processing_functions
#'
#' @family inpure_functions
#'
#' @example
#'    inst/examples/example-xWriteChars.R
#'
#' @rdname xWriteChars
#' @export

xWriteChars <- function (str, strs) {
	# string -> strings
	# write character lines .

	invoking_call <- sys.call()

	insist $ must_not_be_missing(str)

	insist $ must_be_collection(str, invoking_call)
	insist $ must_be_collection(strs, invoking_call)

	str <- unit_to_value(as_atom(str, "character"))
	strs <- unit_to_value(as_typed_vector(strs, "character"))

	strs <- paste0(strs, collapse = '')

	writeLines(strs, str, sep = "")
	invisible (Null)
}
