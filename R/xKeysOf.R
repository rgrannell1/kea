
#' xKeysOf
#'
#' Get the names of a collection.
#'
#' @section Type Signature:
#'     |any| -> <character>
#'
#' @param
#'     coll a collection. The collection to get the names of.
#'
#' @param
#'     ... see above.
#'
#' @return
#'     a character vector.
#'
#' @section Corner Cases:
#'     Returns \character(0) when \bold{coll} is length-zero.
#'
#' @family key_functions
#'
#' @template
#'    Variadic
#'
#' @example
#'    inst/examples/example-xKeysOf.R
#'
#' @rdname xKeysOf
#' @export

xKeysOf <- MakeFun('xKeysOf', function (coll) {


	if (length(coll) == 0) {
		character(0)
	} else {
		collnames <- names(coll)

		# -- names usually returns Null.
		if (length(collnames) == 0) {
			character(0)
		} else {
			collnames
		}
	}

})

#' @rdname xKeysOf
#' @export

xKeysOf_ <- MakeVariadic(xKeysOf, 'coll')
