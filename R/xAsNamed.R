
#' xAsNamed
#'
#' Add names to a collection.
#'
#' @details
#' \bold{xAsNamed} is similar to the in-place assignment
#' function \bold{names<-}, except that it is not an in-place
#' assignment function.
#'
#' @param
#'    strs a collection of strings. The names to add to the
#'    input collection.
#'
#' @param
#'    coll a collection. The collection to add names to.
#'
#' @return
#'    A named list.
#'
#' @section Corner Cases:
#'    Returns the empty list if \bold{coll} is length-zero. Both
#'    duplicated and length-zero names are allowed.
#'
#' @family name_functions
#'
#' @template
#'    Variadic
#'
#' @family container_conversion_functions
#'
#' @example
#'    inst/examples/example-xAsNamed.R
#'
#' @rdname xAsNamed
#' @export

xAsNamed <- function (strs, coll) {
	# Vector string -> Collection any -> [any]
	# add names to a collection.

	invoking_call <- sys.call()

	insist $ must_not_be_missing(strs)
	insist $ must_not_be_missing(coll)

	insist $ must_be_collection(strs, invoking_call)
	insist $ must_be_collection(coll, invoking_call)

	strs <- as_typed_vector(strs, 'character')

	insist $ must_be_equal_length(strs, coll, invoking_call)

	names(coll) <- strs
	coll
}
