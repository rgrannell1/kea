
#' xRepeat
#'
#' Create a collection by repeating another collection several times.
#'
#' @param
#'    num a nonnegative positive number. The number of
#'    times to repeat the collection.
#'
#' @param
#'    coll a collection. The collection to repeat
#'    end to end.
#'
#' @param
#'    ... see above.
#'
#' @return
#'    A list.
#'
#' @section Corner Cases:
#'    Returns the empty list if \bold{coll} is length-zero or num is zero.
#'
#' @family reshaping_functions
#'
#' @template
#'    Variadic
#'
#' @example
#'    inst/examples/example-xRepeat.R
#'
#' @rdname xRepeat
#' @export

xRepeat <- MakeFun(function (num, coll) {
	# number -> Collection any -> [any]
	# repeat a collection several times.

	invoking_call <- sys.call()

	MACRO( Must $ Not_Be_Missing(num) )
	MACRO( Must $ Not_Be_Missing(coll) )

	MACRO( Must $ Be_Collection(num) )
	MACRO( Must $ Be_Collection(coll) )

	num <- unit_to_value(as_atom(num, "numeric"))

	MACRO( Must $ Be_Between(num, 0, Inf))
	MACRO( Must $ Be_Whole(num) )

	if (num == 0) {
		list()
	} else {
		rep(as.list(coll), num)
	}
})

#' @rdname xRepeat
#' @export

xRepeat... <- function (num, ...) {
	xRepeat(num, list(...))
}
