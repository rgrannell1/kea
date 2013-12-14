
#' xIdentity
#'
#' Return an argument without modification.
#'
#' @section Uses:
#'    The identity function is useful for testing identities
#'    (for example, that mapping identity over a collection
#'	  should return the original collection).
#'
#' @param
#'    val an arbitrary value.
#'
#' @return
#'    returns \code{val}.
#'
#' @export

xIdentity <- function (val) {

	invoking_call <- sys.call()

	assert(
		!missing(val), invoking_call,
		exclaim$parameter_missing(val))

	val
}

#' @export

xIdiotBird <- xIdentity

#' @export

xI <- xIdiotBird
