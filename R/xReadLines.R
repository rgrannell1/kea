
#' xReadLines
#'
#' Import each line in a file to an element in a character vector.
#'
#' @section Type Signature:
#'     |character| -> &lt;character>
#'
#' @param
#'    str a length-one character vector. The path to
#'    import files from.
#'
#' @return
#'    A character vector, with one or more elements.
#'
#' @section Corner Cases:
#'   xReadLines ignores missing terminal newlines.
#'
#' @family text_processing_functions
#'
#' @family inpure_functions
#'
#' @example
#'    inst/examples/example-xReadLines.R
#'
#' @rdname xReadLines
#' @export

xReadLines <- MakeFun(function (str) {

	MACRO( Fix(xReadLines, str) )

	MACRO( Must $ Be_Collection(str) )
	MACRO( Must $ Be_File(str) )

	str <- unit_to_value(as_atom(str, "character"))

	text <- try_read(
		readLines(str, warn = False), str, sys.call())

	if (length(text) == 0) {
		character(0)
	} else {
		text
	}
})
