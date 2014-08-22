
#' xTranscribe
#'
#' Format an arbitrary object as a human-readable string.
#'
#' @section Type Signature:
#'     any -> <character>
#'
#' @param val an arbitrary value. The value to convert to a string.
#'
#' @return A length-one character vector.
#'
#' @section Corner Cases:
#'     None.
#'
#' @family text_processing_functions
#'
#' @example
#'    inst/examples/example-xTranscribe.R
#'
#' @rdname xTranscribe
#' @export

xTranscribe <- local({

	add_brackets <- function (str) {
		paste0('[', str, ']')
	}
	comma_delimit <- function (strs) {
		paste(strs, collapse = ', ')
	}






	transcribe <- list()

	transcribe $ `function` <- function (fn) {
		# -- convert a function to a string.

		paste0(deparse(fn), collapse = '\n')
	}

	transcribe $ atomic <- function (coll) {
		# -- convert an atomic vector to a string.

		if (length(coll) == 0) {
			'[]'
		} else {

			coll_names      <- names(coll)
			transcribe_elem <- if (is.null(coll_names)) {

				function (ith) {
					paste( coll[[ith]] )
				}

			} else {

				function (ith) {

					if (nchar( coll_names[[ith]] ) == 0) {
						paste( coll[[ith]] )
					} else {
						paste( coll_names[[ith]], '=', coll[[ith]] )
					}

				}

			}

			add_brackets(comma_delimit(
				vapply (seq_along(coll), transcribe_elem, character(1)) ))

		}

	}

	transcribe $ generic <- function (coll) {
		# -- deparse a collection to string.

		if (length(coll) == 0) {
			'[]'
		} else {

			coll_names      <- names(coll)
			transcribe_elem <- if (is.null(coll_names)) {

				function (ith) {
					xTranscribe( coll[[ith]] )
				}

			} else {

				function (ith) {

					if (nchar( coll_names[[ith]] ) == 0) {
						paste(xTranscribe( coll[[ith]] ))
					} else {
						paste(coll_names[[ith]], '=', xTranscribe( coll[[ith]] ))
					}

				}

			}

			add_brackets(comma_delimit(
				vapply (seq_along(coll), transcribe_elem, character(1)) ))

		}

	}

	transcribe $ environment <- function (env) {
		xTranscribe(as.list(env))
	}

	transcribe $ symbol <- function (sym) {
		paste0(sym)
	}




	MakeFun('xTranscribe', function (val) {

		if (is_atomic(val)) {

			transcribe $ atomic(val)

		} else if (is_generic(val)) {

			transcribe $ generic(val)

		} else if (is.function(val)) {

			transcribe $ `function`(val)

		} else if (is.environment(val)) {

			transcribe $ environment(val)

		} else if (is.symbol(val)) {

			transcribe $ symbol(val)

		} else {

			ddparse(val)
		}

	})

})

