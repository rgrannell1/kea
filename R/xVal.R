
#' xVal
#'
#' Assign a constant value to calling environment.
#'
#' @param
#'    sym a symbol or string.
#' @param
#'    val an arbitrary value.
#'
#' @return
#'    Null; this function is used for side-effects.
#'
#' @section Corner Cases:
#'    overwrites the value referenced by \code{sym}r if the variable
#'    already exists in the parent frame.
#'
#' @family immutable_value_functions
#'
#' @rdname xVal
#' @export

xVal <- function (sym, val) {
	# assign a constant value to the calling environment.

	invoking_call <- sys.call()
	parent_frame <- parent.frame()

	assert(
		!missing(sym), invoking_call,
		exclaim$parametre_missing(sym))

	assert(
		!missing(val), invoking_call,
		exclaim$parametre_missing(val))

	sym <- toString(match.call()$sym)
	sym <- as_typed_vector(sym, 'character', True)

	assign(sym, val, envir = parent_frame)
	lockBinding(sym, parent_frame)
}

#' @export

'%<--%' <- xVal
