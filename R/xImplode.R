
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




	str  <- unit_to_value(as_atom(str, 'character'))
	strs <- as_typed_vector(strs, 'character')

	if (length(strs) == 0) {
		character()
	} else {

		paste(
			strs[nchar(strs) != 0 &
			vapply(strs, length, integer(1), USE.NAMES = False) != 0],
			collapse = str)
	}
})

#' @rdname xImplode
#' @export

xImplode_ <- MakeVariadic(xImplode, 'strs')
