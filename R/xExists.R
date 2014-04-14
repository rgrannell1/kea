
#' xExists
#'
#' Test if any ordered group of arguments from several collections
#' is true for a predicate.
#'
#' @details
#' \bold{xExists} can be used to test if any element of a
#' collection is true (like the base function \bold{all( )}) by
#' simply using it with the identity function.
#'
#' \code{coll <- c(True, False, True, True)}
#'
#' \code{xExists...(xIdentity, coll)}
#'
#' When supplied with a collection of multiple collections the
#' set product of those collections is checked with the predicate.
#' In order for \bold{xExists} to return true at least one element of the
#' set product must return true for a predicate.
#'
#' \code{commutes <- (a : b) := { a + b == b + a }}
#'
#' \code{xExists...(commutes, 1:2, 1:2)}
#'
#' The set product is
#'
#' \code{list(list(1, 1), list(1, 2), list(2, 1), list(2, 2))}
#'
#' Each member of the set product is tested with the
#' \bold{commutes} function
#'
#' \code{list(commutes(1, 1), commutes(1, 2), commutes(2, 1), commutes(2, 2))}
#'
#' \code{list(True, True, True, True)}
#'
#' \code{True}
#'
#' @param
#'    pred an predicate function that takes as many arguments as there
#'    are collections in \bold{colls}.
#'
#' @param
#'    colls a collection of collections. The collection to draw arguments
#'    for \bold{pred} from.
#'
#' @param
#'    ... see above.
#'
#' @return
#'    A boolean value.
#'
#' @section Corner Cases:
#'    If any collection is length zero (or no
#'    collections are given), then logical(0) is returned.
#'
#' @family quantifier_functions
#'
#' @template
#'    Variadic
#'
#' @example
#'    inst/examples/example-xExists.R
#'
#' @rdname xExists
#' @export

xExists <- MakeFun(function (pred, colls) {
	# (... -> logical) -> Collection Collection any -> boolean
	# does there exist any choice of bindings for
	# pred such that pred is true?

	invoking_call <- sys.call()

	MACRO( Must $ Not_Be_Missing(pred) )
	MACRO( Must $ Not_Be_Missing(colls) )

	MACRO( Must $ Be_Fn_Matchable(pred) )
	MACRO( Must $ Be_Collection(colls) )

	MACRO( Must $ Be_Collection_Of_Collections(colls) )

	pred <- match_fn(pred)

	coll_lengths <- vapply(colls, length, integer(1))

	if (length(colls) == 0 || min(coll_lengths) == 0) {
		logical(0)
	} else {

		modulo_iths <- function (n, mods) {
			# get the nth expanded index of an oddly-shaped array.

			assert(n <= prod(mods), invoking_call)
			as.numeric(arrayInd(n, .dim = mods))
		}

		for ( ith in seq_len(prod(coll_lengths)) ) {

			indices <- modulo_iths(ith, coll_lengths)

			tuple <- Map(
				function (coll_ith) {
					choice <- indices[coll_ith]
					colls[[coll_ith]][[choice]]
				},
				seq_along(colls)
			)

			is_match <- do.call(pred, tuple)

			MACRO( Must $ Be_Flag(is_match, pred) )

			if (isTRUE(is_match)) {
				return (True)
			}
		}
		False
	}
})

#' @rdname xExists
#' @export

xExists... <- function (pred, ...) {
	xExists(pred, list(...))
}
