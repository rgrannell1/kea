
#' xRiffle
#'
#' Insert an element in between each element of a list.
#'
#' @section Type Signatures:
#'     any -> |any| -> [any]
#'
#' @param val an arbitrary value. The value to intersperse
#'     between elements in the list.
#'
#' @param coll a collection. The collection to insert elements throughout.
#'
#' @param ... see above.
#'
#' @return A list.
#'
#' @section Corner Cases:
#'    Returns the empty list if \bold{coll} is empty.
#'
#' @section Corner Cases:
#'    Returns the empty list if \bold{coll} is length-zero.
#'
#' @template
#'    Variadic
#'
#' @example
#'    inst/examples/example-xRiffle.R
#'
#' @template
#'    S-Experimental
#'
#' @rdname xRiffle
#' @export

xRiffle <- MakeFun('xRiffle', function (val, coll) {

	# -- drop names as making a list with new elements.
	if (length(coll) == 0) {
		list()
	} else if (length(coll) == 1) {
		list( coll[[1]] )
	} else {

		out      <- lapply(seq_len((2 * length(coll)) - 1), as.null)
		out[[1]] <- coll[[1]]
		jth      <- 2

		for (ith in 2:length(coll)) {
			out[[jth]]     <- val
			out[[jth + 1]] <- coll[[ith]]

			jth <- jth + 2
		}

		out
	}

})

#' @rdname xRiffle
#' @export

xRiffle_ <- MakeVariadic(xRiffle, 'coll')
