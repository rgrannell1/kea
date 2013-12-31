
#' xFoldListl
#'
#' Successively combine a list of values into a single value
#' using a binary function (left to right, with an initial value).
#' Return a list containing each intermediate result, and the final result.
#'
#' @param
#'    fn a binary function that returns a value that
#'    \code{fn} can later take as its right argument.
#'
#' @param
#'    val an arbitrary value.
#'
#' @param
#'    coll a collection.
#'
#' @param
#'    ... see above.
#'
#' @return
#'    a list with its initial element being \code{coll}, and
#'	  containing \code{length(coll) + 1}.
#'
#' @section Corner Cases:
#'	  returns \code{list(val)} if \code{coll} is length-zero.
#'
#' @family folding_functions
#'
#' @template
#'    Variadic
#'
#' @rdname xFoldList
#' @export

xFoldListl <- function (fn, val, coll) {
	# (any -> any -> any) -> any -> Collection any -> [any]
	# scan across list, starting from the right.

	invoking_call <- sys.call()

	assert(
		!missing(fn), invoking_call,
		exclaim$parametre_missing(fn))

	assert(
		!missing(val), invoking_call,
		exclaim$parametre_missing(val))

	assert(
		!missing(coll), invoking_call,
		exclaim$parametre_missing(coll))

	assert(
		is_fn_matchable(fn), invoking_call,
		exclaim$must_be_matchable(
			fn, summate(fn)) )

	assert(
		is_collection(coll), invoking_call,
		exclaim$must_be_collection(
			coll, summate(coll)) )

	fn <- match.fun(fn)

	scanned <- c( val, vector("list", length(coll)) )

	if (length(coll) == 0) {
		val
	} else {
		for (ith in seq_along(coll)) {
			scanned[[ith + 1]] <- try_higher_order(
				fn( scanned[[ith]], coll[[ith]] ),
				invoking_call)
		}
		scanned
	}
}

#' @rdname xFoldList
#' @export

xFoldList <- xFoldListl

#' @rdname xFoldList
#' @export

xFoldListl... <- function (fn, val, ...) {
	xFoldListl(fn, val, list(...))
}

#' @rdname xFoldList
#' @export

xFoldList... <- function (fn, val, ...) {
	xFoldList(fn, val, list(...))
}
