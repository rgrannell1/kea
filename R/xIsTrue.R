
#' xIsTrue
#'
#' Is an value true?
#'
#' @section Type Signature:
#'     any -> &lt;logical>
#'
#' @details
#'     \bold{xIsTrue} is primarily meant for use with conditional
#'     statements like if and while. \bold{xIsTrue} always returns a
#'     length-one true or false value; conditional statements throw an error
#'     if they are given a length zero logical vector.
#'
#'     The below example will throw an error; forall of an empty list is
#'     logical zero, which if cannot handle:
#'
#'     \code{mybool <- xAnyOf(xI, list())}
#'
#'     \code{if (mybool) 1 else 2}
#'
#'     The correct way to test for truth in Kiwi is \bold{xIsTrue}, which will
#'     return false in this case since logical zero isn't the value true.
#'
#'     \code{if (xIsTrue(mybool)) 1 else 2}
#'
#' @param
#'    val an arbitrary value. The value to test for being true.
#'
#' @param
#'    ... see above.
#'
#' @return
#'    A true or false value.
#'
#' @example
#'    inst/examples/example-xIsTrue.R
#'
#' @section Corner Cases:
#'     xIsTrue returns either true or false, to make it
#'     safe for use with if statements. If val is length zero
#'     False is returned.
#'
#' @family value_testing_functions
#'
#' @rdname xIsTrue
#' @export

xIsTrue <- MakeFun(function (val) {

	MACRO( Must $ Not_Be_Missing(val) )

	isTRUE(val)
})
