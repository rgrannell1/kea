
#' xReadChars
#'
#' Import each line in a file to an element in a character vector.
#'
#' @param
#'    str a length-one character vector. The path to
#'    import files from.
#'
#' @return
#'    A character vector, with one or more elements.
#'
#' @section Corner Cases:
#'    xReadChars ignores missing terminal newlines.
#'
#' @family text_processing_functions
#'
#' @family inpure_functions
#'
#' @example
#'    inst/examples/example-xReadChars.R
#'
#' @rdname xReadChars
#' @export

xReadChars <- MakeFun(function (str) {
	# string -> string
	# read a file by characters.

	MACRO( Must $ Not_Be_Missing(str) )

	MACRO( Must $ Be_Collection(str) )
	MACRO( Must $ Be_File(str) )

	str <- unit_to_value(as_atom(str, "character"))

	if (length(text) == 0) {
		character(0)
	} else {
		text <- try_read(
			readLines(str), str, invoking_call)

		chars <- strsplit(
			paste0(text, collapse = '\n'), "")[[1]]

		chars[nchar(chars) > 0]
	}
})
