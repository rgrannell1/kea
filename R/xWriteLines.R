
#' xWriteLines
#'
#' Export a collection of strings as lines in a file.
#'
#' @param
#'    str a length-one character vector. The path to
#'    export to.
#'
#' @param
#'    strs a character vector. Lines to write to a file.
#'
#' @return
#'    A character vector, with one or more elements.
#'
#' @family text_processing_functions
#'
#' @family inpure_functions
#'
#' @example
#'    inst/examples/example-xWriteLines.R
#'
#' @rdname xWriteLines
#' @export

xWriteLines <- function (str, strs) {
	# string -> strings
	# write character lines .

	invoking_call <- sys.call()

	insist $ must_not_be_missing(str)

	insist $ must_be_collection(str, invoking_call)
	insist $ must_be_collection(strs, invoking_call)

	str <- unit_to_value(as_atom(str, "character"))
	strs <- unit_to_value(as_typed_vector(strs, "character"))

	writeLines(strs, str, sep = "\n")
	invisible (Null)
}
