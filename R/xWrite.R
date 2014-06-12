
#' xWrite
#'
#' Write a string to a file.
#'
#' @section Type Signature:
#'     |character| -> {}
#'
#' @param
#'    str1 a length-one character vector. The path to
#'    export to.
#'
#' @param
#'    str2 a length-one character vector. The text to write.
#'
#' @return
#'    Null, as this is a purely side-effectful function.
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

	str  <- unit_to_value(as_atom(str1, "character"))
	str  <- unit_to_value(as_atom(str2, "character"))

	try_write(
		writeLines(strs, str, sep = "\n"), str, sys.call())

	invisible (Null)
})
