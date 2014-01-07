
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
#'    a named list.
#'
#' @section Corner Cases:
#'    returns the empty list if \code{coll} is length-zero.
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

	assert(
		is_collection(strs), invoking_call,
		exclaim$must_be_collection(
			strs, summate(strs)) )

	strs <- as_typed_vector(strs, 'character')

	assert(
		length(strs) == length(coll), invoking_call,
		exclaim$must_have_equal_lengths(strs, coll))

	structure(as.list(coll), names = strs)
}
