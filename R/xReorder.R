
#' xReorder
#'
#' Permute a collection of collections using indices.
#'
#' @section Type Signature:
#'     &lt;number> -> ||any|| -> ||any||
#'
#' @details
#'    \bold{xReorder} allows several collections to be rearranged
#'    in the same way simultaneously.
#'
#'    \code{colls <- list(ids = list(15, 1, 12), ages = list(17, 29, 24), weights = list(80, 82, 76))}
#'
#'    \code{xReorder(xOrderOf( xFirstOf(colls) ), colls)}
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
#'    inst/examples/example-xReorder.R
#'
#' @rdname xReorder
#' @export

xReorder <- MakeFun(function (nums, colls) {

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

#' @rdname xReorder
#' @export

xReorder_ <- MakeVariadic(xReorder, 'colls')
