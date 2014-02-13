
#' xGetKey
#'
#' Return a function that selects a key from a collection.
#'
#' @param
#'    str a string. The key to select from a collection.
#'
#' @return
#'    A unary function that takes a collection.
#'
#' @section Corner Cases:
#'      Returns the empty list if \bold{coll} is length-zero.
#'
#' @family selection_functions
#'
#' @family name_functions
#'
#' @example
#'    inst/examples/example-xGetKey.R
#'
#' @rdname xGetKey
#' @export

xGetKey <- function (str) {
	# Vector string -> (Collection -> [any])
	# Return a function that selects a key from a collection.

	invoking_call <- sys.call()

	insist $ must_not_be_missing(str)

	insist $ must_be_collection(str, invoking_call)

	str <- unit_to_value(as_typed_vector(str, 'character'))

	insist $ must_be_of_length(str, 0:1, invoking_call)

	function (coll) {
		"A function created by xGetKey."
		""
		unname(as.list( coll )[names(coll) == str])
	}
}
