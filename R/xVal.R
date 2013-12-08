
#' xVal
#'
#' Assign a constant value to calling environment.
#'
#' @param str a string or symbol.
#' @param val an arbitrary value.
#'
#' @return Null; this function is used for side-effects.
#'
#' @section Corner Cases:
#'    overwrites the value referenced by \code{str}r if the variable
#' already exists in the parent frame.


#' @export

xVal <- function (str, val) {
	# assign a constant value to the calling environment.

	invoking_call <- sys.call()
	parent_frame <- parent.frame()

	assert(
		!missing(str), invoking_call,
		exclaim$parameter_missing(str))

	assert(
		!missing(val), invoking_call,
		exclaim$parameter_missing(val))

	str <- toString(match.call()$str)
	str <- as_typed_vector(str, 'character', True)

	if (exists(str, envir = parent_frame)) {
		assert(
			!bindingIsLocked(str, parent_frame), invoking_call,
			exclaim$binding_is_locked(str))
	}

	assign(str, val, envir = parent_frame)
	lockBinding(str, parent_frame)
}
