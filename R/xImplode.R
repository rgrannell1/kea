
#' xImplode
#'
#' Concatenate a character vector into a single string using a delimiter.
#'
#' @section Type Signature:
#'     |character| -> |character| -> <character>
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
#'   \bold{strs} is length-zero. \bold{xImplode} does not coerce
#'    its arguments to character.
#'
#' @family text_processing_functions
#'
#' @template
#'    C++
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

	MACRO( Must_Not_Contain_Na(strs) )

	cImplode(str, strs)
})

#' @rdname xImplode
#' @export

xImplode_ <- MakeVariadic(xImplode, 'strs')
