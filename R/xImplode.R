
#' xImplode
#'
#' Concatenate a character vector into a single string using a delimiter.
#'
#' @section Type Signature:
#'     |character| -> |character| -> &lt;character>
#'
#' @param
#'    str a length one character vector. The
#'    string to use as a delimiter.
#'
#' @param
#'    strs a collection of length one character vectors. The
#'    strings to concatenate.
#'
#' @param
#'    ... see above.
#'
#' @return
#'    A length-one character vector.
#'
#' @family text_processing_functions
#'
#' @template
#'    Variadic
#'
#' @example
#'    inst/examples/example-xImplode.R
#'
#' @rdname xImplode
#' @export

xImplode <- MakeFun(function (str, strs) {

	if (length(strs) == 0 || length(str) == 0) {
		character(0)
	} else {
		paste(strs, collapse = str)
	}
})

#' @rdname xImplode
#' @export

xImplode_ <- MakeVariadic(xImplode, 'strs')
