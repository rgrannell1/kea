
#' xRead
#'
#' Read the contents of a file as a single string.
#'
#' @section Type Signature:
#'     |character| -> &lt;character>
#'
#' @param
#'    str a length-one character vector. The path to
#'    import files from.
#'
#' @return
#'    A length-one character vector.
#'
#' @section Corner Cases:
#'   xRead ignores missing terminal newlines.
#'
#' @family text_processing_functions
#'
#' @family inpure_functions
#'
#' @example
#'    inst/examples/example-xRead.R
#'
#' @rdname xRead
#' @export

xRead <- MakeFun(function (str) {



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
