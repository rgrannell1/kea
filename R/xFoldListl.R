
#' xFoldListl
#'
#' Fold a function over a collection from left to right with
#' an initital left value, keeping intermediate values.
#'
#' @param
#'    fn a binary function that returns a value that
#'    \code{fn} can later take as its right argument.
#'
#' @param
#'    init an arbitrary value.
#'
#' @param
#'    coll a collection.
#'
#' @return
#'    a list with its init element being \code{coll}, and
#'	  containing \code{length(coll) + 1}.
#'
#' @section Corner Cases:
#'	  returns \code{list(init)} if \code{coll} is length-zero.
#'
#' @family
#'    higher_order_functions collection_functions
#'
#' @export

xFoldListl <- function (fn, init, coll) {
	# (any -> any -> any) -> any -> Collection any -> [any]
	# scan across list, starting from the right.

	invoking_call <- sys.call()

	assert(
		!missing(fn), invoking_call,
		exclaim$parameter_missing(fn))

	assert(
		!missing(init), invoking_call,
		exclaim$parameter_missing(init))

	assert(
		!missing(coll), invoking_call,
		exclaim$parameter_missing(coll))

	fn <- dearrowise(fn)
	init <- dearrowise(init)
	coll <- dearrowise(coll)

	assert(
		is_fn_matchable(fn), invoking_call,
		exclaim$must_be_matchable(fn))

	assert(
		is_collection(coll), invoking_call,
		exclaim$must_be_collection(coll))

	fn <- match.fun(fn)

	scanned <- c( init, vector("list", length(coll)) )

	if (length(coll) == 0) {
		init
	} else {
		for (ith in seq_along(coll)) {
			scanned[[ith + 1]] <- try_higher_order(
				fn( scanned[[ith]], coll[[ith]] ),
				invoking_call)
		}
		scanned
	}
}

#' @export

xFoldList <- xFoldListl

#' @export

xFoldListl... <- function (fn, init, ...) {
	xFoldListl(fn, init, list(...))
}

#' @export

xFoldList... <- function (fn, init, ...) {
	xFoldList(fn, init, list(...))
}
