
#' xProdSetOf
#'
#' Get the cartesian product of several collections.
#'
#' @param
#'      colls a collection of collections. The collections to
#'      use to take the cartesian product.
#' @param
#'    ... see above.
#'
#' @return
#'      A list of collections, with as many elements per collection as
#'      there are collections in \bold{colls}.
#'
#' @section Corner Cases:
#'      Returns the empty list if \bold{colls} is length-zero.
#'
#' @family combinatoric_functions
#'
#' @family set_functions
#'
#' @template
#'    Variadic
#'
#' @example
#'    inst/examples/example-xProdSetOf.R
#'
#' @rdname xProdSetOf
#' @export

xProdSetOf <- local({

	modulo_iths <- function (num, mods) {
		as.numeric(arrayInd(num, .dim = mods))
	}

	MakeFun(function (colls) {

		MACRO( Must $ Not_Be_Missing(colls) )

		MACRO( Must $ Be_Collection(colls) )
		MACRO( Must $ Be_Collection_Of_Collections(colls) )

		coll_lengths <- vapply(colls, length, integer(1))

		if (length(colls) == 0 || min(coll_lengths) == 0) {
			list()
		} else {

			tuples <- vector(mode = "list", prod(coll_lengths))

			for ( ith in seq_len(prod(coll_lengths)) ) {

				indices <- modulo_iths(ith, coll_lengths)

				tuples[[ith]] <- Map(
					function (coll_ith) {
						choice <- indices[coll_ith]
						colls[[coll_ith]][[choice]]
					},
					seq_along(colls))

			}
			tuples
		}
	})
})


#' @rdname xProdSetOf
#' @export

xProdSetOf_ <- MakeVariadic(xProdSetOf, 'colls')
