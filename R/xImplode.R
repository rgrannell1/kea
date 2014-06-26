
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
#' @section Corner Cases:
#'    Returns the empty character vector when \bold{str} or
#'   \bold{strs} is length-zero.
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

xImplode <- MakeFun('xImplode', function (str, strs) {

	MACRO( Must_Not_Contain_Na(strs) )

	if (length(str) == 0 || length(strs) == 0) {
		character(0)
	} else {
		paste(strs, collapse = str)
	}
})

#' @rdname xImplode
#' @export

xImplode_ <- MakeVariadic(xImplode, 'strs')
