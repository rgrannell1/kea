
#' xPowerSetOf
#'
#' Enumerate every way of subsetting a collection.
#'
#' @section Type Signature:
#'     |any| -> [[any]]
#'
#' @details
#'     \bold{xPowerSetOf} generates the set of all subsets of a collection.
#'     This set has length \bold{2^length(coll)}, so inputs longer than
#'     twenty elements will take a very long time to compute.
#'
#' @param
#'     coll a collection. The collection to return the subsets of.
#'
#' @param
#'    ... see above.
#'
#' @return
#'     A list of lists.
#'
#' @section Corner Cases:
#'     Returns the empty list if \bold{coll} is length-zero.
#'
#' @family combinatoric_functions
#'
#' @family set_functions
#'
#' @template
#'    C++
#'
#' @template
#'    Variadic
#'
#' @example
#'    inst/examples/example-xPowerSetOf.R
#'
#' @rdname xPowerSetOf
#' @export

xPowerSetOf <- MakeFun(function (coll)
	cPowerSetOf(coll)
)

#' @rdname xPowerSetOf
#' @export

xPowerSetOf_ <- MakeVariadic(xPowerSetOf, 'coll')
