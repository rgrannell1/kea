
#' xWrite
#'
#' Write a string to a file.
#'
#' @section Type Signature:
#'     |character| -> { }
#'
#' @param
#'    str1 a length-one character vector. The path to
#'    export to.
#'
#' @param
#'    str2 a length-one character vector. The text to write.
#'
#' @return
#'    Null, as this is a purely side-effectual function.
#'
#' @section Corner Cases:
#'    xWrite does not write to a file if \bold{str1} is empty.
#'
#' @family text_processing_functions
#'
#' @family inpure_functions
#'
#' @example
#'    inst/examples/example-xWrite.R
#'
#' @rdname xWrite
#' @export

xWrite <- MakeFun(function (str1, str2) {

	if (length(str1) == 0) {
		invisible (NULL)
	} else {
		try_write(
			writeLines(str2, str1, sep = "\n"), str1, sys.call())
	}

	invisible (NULL)
})
