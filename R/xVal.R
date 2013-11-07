
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
#' @template glossary
#'
#' @example inst/examples/blank.R
#' @export

xVal <- function (str, val) {
	# assign a constant value to the calling environment.

	parent_call <- sys.call()
	parent_frame <- parent.frame()

	assert(
		!missing(str), parent_call,
		exclaim$parameter_missing(str))

	assert(
		!missing(val), parent_call,
		exclaim$parameter_missing(val))

	str <- toString(match.call()$str)
	str <- coerce_to_typed_vector(str, 'character', True)

	if (exists(str, envir = parent_frame)) {
		assert(
			!bindingIsLocked(str, parent_frame), parent_call,
			exclaim$binding_is_locked(str))
	}

	assign(str, val, envir = parent_frame)
	lockBinding(str, parent_frame)

}
