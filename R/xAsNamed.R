
#' xAsNamed
#'
#' Add names to a collection.
#'
#' @param
#'    strs a collection of strings.
#'
#' @param
#'    coll a collection
#'
#' @return
#'    A named list.
#'
#' @section Corner Cases:
#'    Returns the empty list if \code{coll} is length-zero. Both
#'    duplicated and length-zero names are allowed.
#'
#' @family name_functions
#'
#' @template
#'    Variadic
#'
#' @rdname xAsNamed
#' @export

xAsNamed <- function (strs, coll) {
	# Vector string -> Collection any -> [any]
	# add names to a collection.

	invoking_call <- sys.call()

	assert(
		!missing(strs), invoking_call,
		exclaim$parametre_missing(strs))

	assert(
		!missing(coll), invoking_call,
		exclaim$parametre_missing(coll))

	insist$must_be_collection(strs, invoking_call)
	insist$must_be_collection(coll, invoking_call)

	strs <- as_typed_vector(strs, 'character')

	insist$must_be_equal_length(strs, coll, invoking_call)

	structure(as.list(coll), names = strs)
}
