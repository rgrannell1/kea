
#' xVal
#'
#' Assign a constant value to calling environment.
#'
#' @param
#'    str a string or symbol.
#' @param
#'    val an arbitrary value.
#'
#' @return
#'    Null; this function is used for side-effects.
#'
#' @section Corner Cases:
#'    overwrites the value referenced by \code{str}r if the variable
#'    already exists in the parent frame.
#'
#' @family immutable_value_functions
#'
#' @rdname xVal
#' @export

xVal <- function (str, val) {
	# assign a constant value to the calling environment.

	invoking_call <- sys.call()
	parent_frame <- parent.frame()

	assert(
		!missing(str), invoking_call,
		exclaim$parametre_missing(str))

	assert(
		!missing(val), invoking_call,
		exclaim$parametre_missing(val))

	str <- toString(match.call()$str)
	str <- as_typed_vector(str, 'character', True)

	assign(str, val, envir = parent_frame)
	lockBinding(str, parent_frame)
}

#' @export

'%<--%' <- xVal
