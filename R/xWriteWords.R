
#' xWriteWords
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
#'    A character vector, with one or more elements.
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
	# string -> strings
	# write character lines .

	MACRO( Must $ Not_Be_Missing(str) )
	MACRO( Must $ Not_Be_Missing(strs) )

	MACRO( Must $ Be_Collection(str) )
	MACRO( Must $ Be_Collection(strs) )

	str <- unit_to_value(as_atom(str, "character"))
	strs <- unit_to_value(as_typed_vector(strs, "character"))

	strs <- paste0(strs, collapse = ' ')

	try_write(
		writeLines(strs, str, sep = " "), str, sys.call())

	invisible (Null)
})
