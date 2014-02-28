
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
#' @family character_functions
#'
#' @family inpure_functions
#'
#' @example
#'    inst/examples/example-xReadLines.R
#'
#' @rdname xReadLines
#' @export

xReadLines <- function (str) {
	# string -> string

	invoking_call <- sys.call()

	insist $ must_not_be_missing(str)
	insist $ must_be_collection(str, invoking_call)
	insist $ must_be_existing_file(str, invoking_call)

	str <- unit_to_value(as_atom(str, "character"))

	text <- readLines(str, warn = False)

	if (length(text) == 0) {
		character(0)
	} else {
		text
	}
}
