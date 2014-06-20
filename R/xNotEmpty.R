
#' xNotEmpty
#'
#' Is a collection not length-zero?
#'
#'
#' @section Type Signature:
#'     any -> &lt;logical>
#'
#' @param
#'    coll a collection. The value to test for
#'    having non-zero length.
#'
#' @param
#'    ... see above.
#'
#' @return
#'    A boolean value.
#'
#' @section Corner Cases:
#'
#' @template
#'    Variadic
#'
#' @example
#'    inst/examples/example-xNotEmpty.R
#'
#' @family value_testing_functions
#'
#' @rdname xNotEmpty
#' @export

xNotEmpty <- MakeFun('xNotEmpty', function (coll) {
	isTRUE(length(coll) != 0)
})

#' @rdname xNotEmpty
#' @export

xNotEmpty_ <- MakeVariadic(xNotEmpty, 'coll')
