
# --------------------- shorthand logical functions --------------------- #

Null <- NULL
Na <- NA
True <- TRUE
False <- FALSE

Truth <- function (...) {
	True
}
Falsity <- function (...) {
	False
}
Moot <- function (...) {
	Na
}


# --------------------- misc. tools --------------------- #

one_of <- function (coll) {
	# select a single value from a collection.

	ith <- sample(seq_along(coll), size = 1)
	coll[[ith]]
}

'%equals%' <- function (a, b) {
	# are two values identical?

	identical(a, b)
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

"%+%" <- function (x, y) {
	# javascript-style string concatenation.

	paste0(x, y, sep = "")
}

# --------------------- environment manipulation --------------------- #

object <- function () {
	# construct an empty environment.

	new.env(parent = emptyenv())
}

join_env <- function (x, y) {
	# do not use this often; it's a very slow
	# way of joining two environments.

	if (missing(x)) {
		exclaim$parameter_missing(x)
	}
	if (missing(y)) {
		exclaim$parameter_missing(y)
	}

	as.environment( c(as.list( x ), as.list( y )) )
}

# --------------------- the arrow container --------------------- #

is_arrow <- function (val) {
	# is a function an arrow object?

	if (missing(val)) {
		exclaim$parameter_missing(val)
	}

	class(val) == "arrow"
}

dearrowise <- function (val) {
	# if a value is in an arrow object, take it out.
	# otherwise do nothing.

	if (is_arrow(val)) {
		val$x()
	} else {
		val
	}
}




# --------------------- property tests --------------------- #

is_fn_matchable <- function (val) {
	# is a value a function or matchable as a function?

	is.function(val) || is.symbol(val) ||
	(is.character(val) && length(val) == 1)
}

is_collection <- function (val) {
	# is a value a pairlist, list or typed vector?

	is.vector(val) || is.pairlist(val)
}

is_recursive <- function (val) {
	is.list(val) || is.pairlist(val)
}





# --------------------- coercion functions --------------------- #

as_parametres <- function (names) {
	# takes a string of names and converts them to
	# a pairlist of formals with no defaults.

	structure(
		replicate(length(names), quote(expr=)),
		names = names
	)
}

as_typed_vector <- function (coll, mode, value_unit = False) {
	# coerces an R vector (pairlist, list, or typed vector)
	# to another mode, if the vector is homogenously typed.
	# this makes list("a") ~ "a", making arrow more generic.

	coll_symbol <- match.call()$coll

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

	if (!is_homogenous) {
		stop(exclaim$type_coersion_failed(coll_symbol, mode))
	}

	coll <- as.vector(coll, mode)

	# coerce the length-zero collection to a unit-value.
	# this doesn't always make sense to do.

	if (value_unit && length(coll) == 0) {
		if (is.numeric(coll)) {
			0
		} else if (is.character(coll)) {
			""
		} else if (is.logical(coll)) {
			False
		} else if (is.raw(coll)) {
			as.raw(00)
		} else {
			stop("")
		}

	} else {
		coll
	}
}

try_higher_order <- function (expr, invoking_call) {
	# provide a good error message if a higher-order function
	# fails because the user provided a dodgy function.

	tryCatch(
		expr,
		warning = function (warn) {

			assert(
				False, invoking_call,
				exclaim$warning_higher_order( invoking_call[[1]], warn ))

		},
		error = function (err) {

			assert(
				False, invoking_call,
				exclaim$error_higher_order( invoking_call[[1]], err  ))

		}
	)
}

# --------------------- testing & message functions --------------------- #


assert <- function (expr, invoking_call, message) {
	# does an expression evaluate to true?
	# if not, throw a lovely error.

	args <- as.list(match.call())[-1]

	if (!expr) {
		call <- if (missing(invoking_call)) {
			'assert()'
		} else {
			if (is.character(invoking_call)) {
				invoking_call
			} else {
				paste0(deparse(invoking_call), collapse = '')
			}
		}

		if (missing(message)) {

			stop(
				call,
				": the assertion\n",
				"    ", paste0(deparse(args$expr), collapse = ''), "\n",
				"failed.",
				call. = False)

		} else {
			stop(call, ": ", message, call. = False)
		}
	}
}

ith_suffix <- function (num) {
	# takes a number i, adds the
	# appropriate suffix (ith, ind, ist)
	# useful for error messages.

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

modify_call <- function (invoking_call) {

}