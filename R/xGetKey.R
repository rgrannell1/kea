
#' xGetKey
#'
#' Return a function that selects a key from a collection.
#'
#' @param
#'    str a string.
#'
#' @return
#'    A unary function that takes a collection.
#'
#' @section Corner Cases:
#'      Returns the empty list if \code{coll} is length-zero.
#'
#' @family selection_functions
#'
#' @family name_functions
#'
#' @rdname xGetKey
#' @export

xGetKey <- function (str) {
	# Vector string -> (Collection -> [any])
	# Return a function that selects a key from a collection.

	invoking_call <- sys.call()

	assert(
		!missing(str), invoking_call,
		exclaim$parametre_missing(str))

	str <- as_typed_vector(str, 'character', True)

	function (coll) {
		"A function created by xGetKey."
		""
		unname(as.list( coll )[names(coll) == str])
	}
}
