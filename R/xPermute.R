
#' xPermute
#'
#' Permute a collection of collections using indices.
#'
#' @details
#'    \bold{xPermute} allows several collections to be rearranged
#'    in the same way simultaneously.
#'
#'    \code{colls <- list(ids = list(15, 1, 12), ages = list(17, 29, 24), weights = list(80, 82, 76))}
#'
#'    \code{xPermute(xOrderOf( xFirstOf(colls) ), colls)}
#'
#' @param
#'    nums a collection of whole numbers. The indices
#'    by which to permute the input collections.
#'
#' @param
#'    colls several collections of equal length to \bold{nums}. The
#'    collections to permute.
#'
#' @param
#'    ... see above.
#'
#' @return
#'    A list of collections.
#'
#' @section Corner Cases:
#'    returns the empty list is \bold{nums} is length-zero.
#'
#' @family combinatoric_functions
#'
#' @family set_functions
#'
#' @template
#'    Variadic
#'
#' @example
#'    inst/examples/example-xPermute.R
#'
#' @rdname xPermute
#' @export

xPermute <- MakeFun(function (nums, colls) {
	# Vector integer -> [[any]] -> [[any]]
	# Permute several collections using indices.

	invoking_call <- sys.call()

	MACRO( Must $ Not_Be_Missing(nums) )
	MACRO( Must $ Not_Be_Missing(colls) )

	MACRO( Must $ Be_Collection(nums) )
	MACRO( Must $ Be_Collection(colls) )

	nums <- as_typed_vector(nums, 'numeric')

	MACRO( Must $ Be_Collection_Of_Collections(colls) )

	if (length(nums) == 0) {
		list()
	} else {
		lapply(colls, function (permutable) {
			as.list(permutable[nums])
		})
	}
})

#' @rdname xPermute
#' @export

xPermute... <- function (nums, ...) {
	xPermute(nums, list(...))
}
