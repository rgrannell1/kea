
#' xFoldr
#'
#' Successively combine a list of values into a single value
#' using a binary function (right to left, with an initial value).
#'
#' @param
#'    fn a binary function that returns a value that
#'	  \code{fn} can later take as its right argument
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
#'    an arbitrary value, depending on the function \code{fn}.
#'
#' @section Corner Cases:
#'	  returns \code{val} if \code{coll} is length-zero.
#'
#' @family folding_functions
#'
#'
#' @template
#'    Variadic
#'
#' @template
#'    Return
#'
#' @rdname xFoldr
#' @export

xFoldr <- function (fn, val, coll) {
	# (any -> any -> any) -> any -> Collection any -> any
	# fold a list, starting from the right

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

	fn <- match_fn(fn)

	if (length(coll) == 0) {
		val
	} else {

		callCC(function (Return) {

			if (!is.primitive(fn)) {
				clone_env <- new.env(parent = environment(fn))
				clone_env$Return <- Return

				environment(fn) <- clone_env
			}

			for (ith in length(coll):1) {

				val <- try_higher_order(
					fn( coll[[ith]], val ),
					invoking_call)

			}
			val
		})

	}
}

#' @rdname xFoldr
#' @export

xFoldr... <- function (fn, val, ...) {
	xFoldr(fn, val, list(...))
}
