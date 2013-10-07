
# I don't want all caps to frighten timid users

Null <- NULL
Na <- NA
True <- TRUE
False <- FALSE

object <- function () {
	new.env(parent = emptyenv())
}

call_with_params <- function (name, fn) {
	# string -> function -> call
	# create call for a function with
	# the arguments of another function.
	
	as.call(
		lapply(
			c(name, names(xFormals(fn)) ),
			as.symbol))
}

assert <- function (bool, pcall) {
	args <- as.list(match.call())[-1]

	if (!bool) {
		call <- if (missing(pcall)) {
			'assert()'
		} else {
			paste0(deparse(pcall), collapse = '')
		}
		stop(
			call,
			": the assertion\n",
			"    ", paste0(deparse(args$bool), collapse = ''), "\n",
			"failed.",
			call. = False)
	}
}

# not included arbitrarily; I sometimes use this constant.
#' @export

tau <- 6.2831853071795864769252867

cc <- list

ith_suffix <- function (num) {

	last <- as.numeric(substr(
		toString(num), 
		nchar(toString(num)), 
		nchar(toString(num)) ))

	suffix <- 
	if (num == 2) {
		"nd"
	} else if (num == 3) { 
		"rd"
	} else if (num == 11 || num == 12 || num == 13) {
		"th"
	} else if (last == 1) {
		"st"
	} else if (last == 2) {
		"nd"
	} else if (last == 3) {
		"rd"
	} else {
		"th"
	}
	paste0(num, suffix)
}

"+" <- function (x, y) {
	if (is.character(x) | is.character(y)) {
		return (paste0(x , y, sep = ""))
	} else {
		.Primitive("+")(x,y)
	}
}
