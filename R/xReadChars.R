
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
#'    xReadChars ignores missing terminal newlines.
#'
#' @family character_functions
#'
#' @example
#'    inst/examples/example-xReadChars.R
#'
#' @rdname xReadLines
#' @export

xReadChars <- function (str) {
	# string -> string
	# read a file by characters.

	invoking_call <- sys.call()

	insist $ must_not_be_missing(str)
	insist $ must_be_collection(str, invoking_call)

	str <- unit_to_value(as_atom(str, "character"))

	if (length(text) == 0) {
		character(0)
	} else {
		chars <- strsplit(
			paste0(readLines(str, warn = False), collapse = '\n'), "")[[1]]

		chars[nchar(chars) > 0]
	}
}
