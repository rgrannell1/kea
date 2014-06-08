
#' xWriteWords
#'
#' Export a collection of strings as a single line in a file.
#'
#' @section Type Signature:
#'     |character| -> {}
#'
#' @param
#'    str a length-one character vector. The path to
#'    export to.
#'
#' @param
#'    strs a character vector. Lines to write to a file.
#'
#' @return
#'    Null, as this is a purely side-effectful function.
#'
#' @family text_processing_functions
#'
#' @family inpure_functions
#'
#' @example
#'    inst/examples/example-xWriteWords.R
#'
#' @rdname xWriteWords
#' @export

xWriteWords <- MakeFun(function (str, strs) {

	MACRO( Fix(xWriteWords, str, strs) )

	MACRO( Must $ Be_Collection(str) )
	MACRO( Must $ Be_Collection(strs) )

	str <- unit_to_value(as_atom(str, "character"))
	strs <- unit_to_value(as_typed_vector(strs, "character"))

	# collapse to the final string before writing.
	strs <- paste0(strs, collapse = ' ')

	try_write(
		writeLines(strs, str, sep = " "), str, sys.call())

	invisible (Null)
})
