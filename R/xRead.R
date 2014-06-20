
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
#'    xRead ignores missing terminal newlines. If \bold{str} is
#'    is length-zero the empty character-vector is returned.
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

xRead <- MakeFun('xRead', function (str) {

	MACRO( Must_Be_File(str) )

	if (length(str) == 0) {
		character(0)
	} else {

		MACRO( Must_Be_File(str) )

		text <- try_read(
			readLines(str, warn = False), str, sys.call()
		)

		paste0(text, collapse = '\n')
	}

})
