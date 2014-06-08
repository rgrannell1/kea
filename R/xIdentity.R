
#' xIdentity
#'
#' Return an argument without modification.
#'
#' @section Type Signature:
#'     any -> any
#'
#' @param
#'    val an arbitrary value.
#'
#' @return
#'    Returns \bold{val}.
#'
#' @family basic_functions
#'
#' @example
#'    inst/examples/example-xIdentity.R
#'
#' @rdname xIdentity
#'
#' @export

xIdentity <- MakeFun(function (val) {

	MACRO( Fix(xIdentity, val) )

	val
})

#' @rdname xIdentity
#' @export

xI <- xIdentity
