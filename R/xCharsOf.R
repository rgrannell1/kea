
#' xCharsOf
#'
#' Get the number of characters in a string.
#'
#' @section Type Signature:
#'     |character| -> <character>
#'
#' @param
#'     str a length-one character vector. The string to
#'     get the numbers of characters in.
#'
#' @return
#'     An integer.
#'
#' @section Corner Cases:
#'    none.
#'
#' @family text_processing_functions
#'
#' @example
#'    inst/examples/example-xCharsOf.R
#'
#' @rdname xCharsOf
#' @export

xCharsOf <- MakeFun(function (str)
	nchar(str)
)
