
#' xReadLines
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
	# string -> string

	invoking_call <- sys.call()

	MACRO( Must $ Not_Be_Missing(str) )

	MACRO( Must $ Be_Collection(str) )
	MACRO( Must $ Be_File(str) )

	str <- unit_to_value(as_atom(str, "character"))

	text <- try_read(
		readLines(str, warn = False), str, invoking_call)

	if (length(text) == 0) {
		character(0)
	} else {
		text
	}
})
