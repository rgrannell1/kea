
#' xReadWords
#'
#' Import each word in a file to an element in a character vector.
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
#'    xReadWords ignores missing terminal newlines.
#'
#' @family text_processing_functions
#'
#' @family inpure_functions
#'
#' @example
#'    inst/examples/example-xReadWords.R
#'
#' @rdname xReadWords
#' @export

xReadWords <- MakeFun(function (str) {

	MACRO( Must $ Not_Be_Missing(str) )

	MACRO( Must $ Be_Collection(str) )
	MACRO( Must $ Be_File(str) )

	str <- unit_to_value(as_atom(str, "character"))

	if (length(text) == 0) {
		character(0)
	} else {
		text <- try_read(
			readLines(str, warn = False), str, sys.call())

		words <- strsplit(
			paste0(text, collapse = '\n'), "[ \n\t]+")[[1]]

		words[nchar(words) > 0]
	}
})
