
#' xRecycle
#'
#' Make every subcollection in a collection the same
#' length by cyclically reusing elements.
#'
#' @details
#'
#'     xRecycle takes a ragged collection - a collection of collections
#'     of varying length - and recycles elements to make the shorter
#'     collections the same length as the longest collection.
#'
#'     \code{xRecycle...( list('a', 'b'), 1:3 )}
#'
#'     \code{list(list("a", "b", "a"), list(1L, 2L, 3L))}
#'
#' @param
#'     colls a collection of collections. The collection
#'     to convert to a non-ragged form.
#'
#' @param
#'    ... see above.
#'
#' @return
#'    A list of lists.
#'
#' @section Corner Cases:
#'    Returns the empty list if \bold{colls} is length-zero, or if a
#'    collection in \bold{colls} is length-zero.
#'
#' @family reshaping_functions
#'
#' @template
#'    Variadic
#'
#' @rdname xRecycle
#' @export

xRecycle <- MakeFun(function (colls) {
	# recycle elements of a staggered-array

	invoking_call <- sys.call()

	MACRO( arrow ::: Must $ Not_Be_Missing(colls) )

	MACRO( arrow ::: Must $ Be_Collection(colls) )

	insist $ must_be_collection_of_collections(colls, invoking_call)

	coll_lens <- vapply(colls, length, integer(1))

	if (length(colls) == 0 || 0 %in% coll_lens) {
		list()
	} else {

		upper <- max(coll_lens)

		lapply(colls, function (coll) {

			lapply(1:upper, function (ith) {
				mod_index <- ((ith - 1) %% length(coll))  + 1
				coll[[mod_index]]
			})
		})

	}
})

#' @rdname xRecycle
#' @export

xRecycle... <- function (...) {
	xRecycle(list(...))
}
