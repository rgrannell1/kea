
# I don't want all caps to frighten timid users

Null <- NULL
Na <- NA
True <- TRUE
False <- FALSE

object <- function () {
	new.env(parent = emptyenv())
}

as_parametres <- function (names) {
	structure(
		replicate(length(names), quote(expr=)),
		names = names
	)
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

assert <- function (bool, pcall, message) {
	args <- as.list(match.call())[-1]

	if (!bool) {
		call <- if (missing(pcall)) {
			'assert()'
		} else {
			if (is.character(pcall)) {
				pcall
			} else {
				paste0(deparse(pcall), collapse = '')				
			}
		}

		if (missing(message)) {

			stop(
				call,
				": the assertion\n",
				"    ", paste0(deparse(args$bool), collapse = ''), "\n",
				"failed.",
				call. = False)
						
		} else {
			stop(call, ": ", message, call. = False)
		}
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

"%+%" <- function (x, y) {
	paste0(x, y, sep = "")
}

join_env <- function (a, b) {

	as.environment( c(as.list( a ), as.list( b )) )
}

is_arrow <- function (val) {
	class(val) == "arrow"
}

dearrowise <- function (val) {
	if (is_arrow(val)) {
		val$x()
	} else {
		val
	}
}

is_fn_matchable <- function (val) {
	is.function(val) || is.symbol(val) || 
	(is.character(val) && length(val) == 1)
}

is_collection <- function (val) {
	is.vector(val) || is.pairlist(val)
}

coerce_to_vector <- function (coll, mode) {

	types <- list(
		logical = 
			is.logical,
		integer = 
			is.integer,
		double = 
			is.double,
		numeric =
			is.numeric,
		character = 
			is.character,
		raw = 
			is.raw
	)

	type_test <- types[[mode]]
	is_homogenous <- all(sapply(coll, type_test))

	if (is_homogenous) {
		as.vector(coll, mode)
	} else {
		stop(exclaim$type_coersion_failed(coll, mode))
	}
}








