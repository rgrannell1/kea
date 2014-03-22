
#' xGroupBy
#'
#' Group elements of a collection by the output of a function applied to each element.
#'
#' @details
#'     xGroupBy allows the grouping of a heterogenous dataset into groups of elements
#'     similar in some way.
#'
#' @param
#'     fn a unary function. The function returning the value to group its input by.
#'
#' @param
#'     coll a collection. The elements to group by a property.
#'
#'
#'
#'
#' @param
#'    ... see above.
#'
#' @return
#'    A list
#'
#' @section Corner Cases:
#'    If \bold{coll} is a empty collection the empty list is returned.
#'
#' @template
#'    Variadic
#'
#' @family reshaping_functions
#'
#' @example
#'    inst/examples/example-xGroupBy.R
#'
#' @rdname xGroupBy
#' @export

xGroupBy <- MakeFun(function (fn, coll) {
	# (a -> b) -> Collection any -> [any]
	# group elements of a collection by its map

	invoking_call <- sys.call()

	MACRO( Must $ Not_Be_Missing(fn) )
	MACRO( Must $ Not_Be_Missing(coll) )

	MACRO( Must $ Be_Fn_Matchable(fn) )
	MACRO( Must $ Be_Collection(coll) )

	fn <- match_fn(fn)

	if (length(coll) == 0) {
		list()
	} else {
		groups <- list()

		for (ith in seq_along(coll)) {
			group_found <- False

			elem <- coll[[ith]]
			map <- fn(elem)

			for (jth in seq_along(groups)) {

				if ( identical(map, groups[[jth]][[1]] ) ) {

					groups[[jth]][[2]] <- c(groups[[jth]][[2]], elem)

					group_found <- True
				}
			}

			if (!group_found) {
				groups <- c( groups, list(list(map, list(elem ))) )
			}
		}
		groups
	}
})

#' @rdname xGroupBy
#' @export

xGroupBy... <- function (fn, ...) {
	xGroupBy(fn, list(...))
}
