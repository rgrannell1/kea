
# To Developers,
#
# Every sufficiently large utility library will contain its own utility library.
# These functions are required to reduce repetition of code when implementing arrow
# functions.
#
# Most utilities are obvious, but I will outline key utilities.
#
# assert:
#     Assert checks if a proposition is true, and if it fails
#     throws a helpful error (see say.R).
#
# as_typed_vector:
#     A function to try convert a list of values to a typed vector.
#     A list of integers should be interconvertable to an integer vector,
#     if required. This function is used to make sure arrow functions are
#     agnostic to the difference between typed and generic vectors.
#
# try_higher_order:
#     Arrow functions almost always throw errors that can be quickly debugged.
#     Unfortunately,  higher-order functions throw a spanner in the works by
#     allowing user-written functions to throw odd errows that cannot be
#     located easily.
#
#     try_higher_order is a variant of tryCatch that allows
#     extra-information to be added to the errors produced by higher-order functions.
#
# profile_object:
#     Debugging is annoying, so I'd like to provide the attributes of an error
#     causing object.
#
#


# --------------------- shorthand logical functions --------------------- #

Na <- NA
Null <- NULL
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
	# coll [any] -> any
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

Object <- function () {
	# construct an empty environment.

	new.env(parent = emptyenv())
}

join_env <- function (x, y) {
	# do not use this often; it's a very slow
	# way of joining two environments.

	if (missing(x)) {
		exclaim$parametre_missing(x)
	}
	if (missing(y)) {
		exclaim$parametre_missing(y)
	}

	as.environment( c(as.list( x ), as.list( y )) )
}

# --------------------- the arrow container --------------------- #

is_arrow <- function (val) {
	# is a function an arrow object?

	if (missing(val)) {
		exclaim$parametre_missing(val)
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
	# expression -> call -> any
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
			message <- paste0(
				strwrap(message, width = 70), collapse = '\n')
			stop(call, ": ", message, call. = False)
		}
	}
}

ith_suffix <- function (num) {
	# number -> string
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

profile_object <- local({
	# Returns a string of information about an input object.

	profile <- Object()

	# --- A --- #
	# --- B --- #
	# --- C --- #

	profile$character_vector <-
		function (obj) {

			traits <- list(
				length =
					length(obj),
				no_empty =
					length(which(nchar(obj) == 0)),
				classes =
					deparse(class(obj))
			)

			"\n\n" %+% "[ properties of the error-causing character vector ]" %+% "\n\n" %+%

			"c(length = " %+% traits$length %+% ", " %+%
			"no_empty = " %+% traits$no_empty %+% ", " %+%
			"classes = " %+% traits$classes %+% ")"

		}

	profile$closure <-
		function (obj) {

			traits <- list(
				function_type =
					is.primitive(obj),
				arity =
					if (is.primitive(obj)) {
						length( head(as.list(args(obj)), -1) )
					} else {
						length(formals(obj))
					},
				classes =
					deparse(class(obj))
			)

			"\n\n" %+% "[ properties of the error-causing function ]" %+% "\n\n" %+%
			"c(primitive = " %+% traits$function_type %+% ", " %+%
			"arity = " %+% traits$arity %+% ", " %+%
			"classes = " %+% traits$classes %+% ")"

		}

	# --- D --- #

	profile$data_frame <-
		function (obj) {

		}

	profile$default <-
		function (obj) {

			traits <- list(
				classes =
					deparse(class(obj))
			)

			"\n\n" %+% "[ properties of the error-causing value ]" %+% "\n\n" %+%
			"c(classes = " %+% traits$classes %+% ")"

		}

	profile$double_vector <-
		function (obj) {

			traits <- list(
				length =
					length(obj),
				no_positive =
					length(which(obj[ !(is.na(obj)) ] > 0)),
				no_zero =
					length(which(obj[ !(is.na(obj)) ] == 0)),
				no_negative =
					length(which(obj[ !(is.na(obj)) ] < 0)),
				no_na =
					length( which(is.na(obj)) ),
				no_nan =
					length( which(is.nan(obj)) ),
				no_whole =
					local({
						roundable <- obj[ !(is.na(obj) | is.nan(obj) | is.infinite(obj)) ]
						length(which(round(roundable) == roundable))
					}),
				no_infinite =
					length( which(is.integer(obj)) ),
				classes =
					deparse(class(obj))
			)

			"\n\n" %+% "[ properties of the error-causing double vector ]" %+% "\n\n" %+%

			"c(length = " %+% traits$length %+% ", " %+%
			"no_empty = " %+% traits$no_empty %+% ", " %+%
			"classes = " %+% traits$classes %+% ")"

		}

	# --- E --- #
	# --- F --- #

	profile$factor <-
		function (obj) {

			traits <- list(
				ordered_factor =
				is.ordered(obj),
				levels =
					length(levels(obj)),
				length =
					length(obj),
				classes =
					deparse(class(obj))
			)

			"\n\n" %+% "[ properties of the error-causing factor ]" %+% "\n\n" %+%

			"c(ordered_factor = " %+% traits$ordered %+% ", " %+%
			"levels = " %+% traits$levels %+% ", " %+%
			"length = " %+% traits$length %+% ", " %+%
			"classes = " %+% traits$classes %+% ")"

		}

	# --- G --- #

	profile$generic_vector <-
		function (obj) {

		}

	# --- H --- #
	# --- I --- #

	profile$integer_vector <-
		function (obj) {

			traits <- list(
				length =
					length(obj),
				no_positive =
					length(which(obj[ !(is.na(obj)) ] > 0)),
				no_zero =
					length(which(obj[ !(is.na(obj)) ] == 0)),
				no_negative =
					length(which(obj[ !(is.na(obj)) ] < 0)),
				no_na =
					length( which(is.na(obj)) ),
				classes =
					deparse(class(obj))
			)

			"\n\n" %+% "[ properties of the error-causing integer vector ]" %+% "\n\n" %+%

			"c(length = " %+% traits$length %+% ", " %+%
			"negative_values = " %+% traits$no_negative %+% ", " %+%
			"zero_values = " %+% traits$no_zero %+% ", " %+%
			"positive_values = " %+% traits$no_positive %+% ", " %+%
			"na_values = " %+% traits$no_na %+% ", " %+%
			"nan_values = " %+% traits$no_nan %+% ", " %+%
			"classes = " %+% traits$classes %+% ")"

		}

	# --- J --- #
	# --- K --- #
	# --- L --- #

	profile$logical_vector <-
		function (obj) {

			traits <- list(
				length =
					length(obj),
				no_na =
					length( which(is.na(obj)) ),
				no_true =
					length(which(obj)),
				no_false =
					length(which(!obj)),
				classes =
					deparse(class(obj))
			)


			"\n\n" %+% "[ properties of the error-causing logical vector ]" %+% "\n\n" %+%

			"c(length = " %+% traits$length %+% ", " %+%
			"na_values = " %+% traits$no_na %+% ", " %+%
			"true_values = " %+% traits$no_true %+% ", " %+%
			"false_values = " %+% traits$no_false %+% ", " %+%
			"classes = " %+% traits$classes %+% ")"

		}

	# --- M --- #

	profile$matrix <-
		function (obj) {

			traits <- list(
				nrow =
					nrow(obj),
				ncol =
					ncol(obj),
				type =
					deparse(typeof(obj)),
				classes =
					deparse(class(obj))
			)

			"\n\n" %+% "[ properties of the error-causing matrix ]" %+% "\n\n" %+%
			"c(nrow = " %+% traits$nrow %+% ", " %+%
			"ncol = " %+% traits$ncol %+% ", " %+%
			"type = " %+% traits$type %+% ", " %+%
			"classes = " %+% traits$classes %+% ")"

		}

	# --- N --- #

	profile$null <-
		function (obj) {

			"\n\n" %+% "[ properties of ther error-causing object ]" %+% "\n\n" %+%
			"NULL"
		}

	# --- O --- #
	# --- P --- #
	# --- Q --- #
	# --- R --- #

	profile$raw_vector <-
		function (obj) {

			traits <- list(
				length =
					length(obj),
				classes =
					deparse(class(obj))
			)


			"\n\n" %+% "[ properties of the error-causing raw vector ]" %+% "\n\n" %+%

			"c(length = " %+% traits$length %+% ", " %+%
			"classes = " %+% traits$classes %+% ")"

	profile$character_vector <-
		function (obj) {

			traits <- list(
				length =
					length(obj),
				no_empty =
					length(which(nchar(obj) == 0)),
				classes =
					deparse(class(obj))
			)

			"\n\n" %+% "[ properties of the error-causing character vector ]" %+% "\n\n" %+%

			"c(length = " %+% traits$length %+% ", " %+%
			"no_empty = " %+% traits$no_empty %+% ", " %+%
			"classes = " %+% traits$classes %+% ")"

		}

		}

	# --- S --- #
	# --- T --- #

	profile$character_vector <-
		function (obj) {

			traits <- list(
				length =
					length(obj),
				no_empty =
					length(which(nchar(obj) == 0)),
				classes =
					deparse(class(obj))
			)

			"\n\n" %+% "[ properties of the error-causing character vector ]" %+% "\n\n" %+%

			"c(length = " %+% traits$length %+% ", " %+%
			"no_empty = " %+% traits$no_empty %+% ", " %+%
			"classes = " %+% traits$classes %+% ")"

		}

	# -- U --- #
	# --- V --- #
	# --- W --- #
	# --- X --- #
	# --- Y --- #
	# --- Z --- #

	# the set of implemented object summaries.

	function (obj) {
		# return the appropriate string summary,
		# depending on the type of object supplied.

		response_pairs <- list(
			list(
				is.function,
				profile$closure),

			list(
				is.null,
				profile$null),

			list(
				is.factor,
				profile$factor),

			list(
				function (x) is.list(x) || is.pairlist(x),
				profile$generic_vector),

			list(
				function (x) is.logical(x) && is.vector(x),
				profile$logical_vector),
			list(
				function (x) is.raw(x) && is.vector(x),
				profile$raw_vector),
			list(
				function (x) is.integer(x) && is.vector(x),
				profile$integer_vector),
			list(
				function (x) is.double(x) && is.vector(x),
				profile$double_vector),
			list(
				function (x) is.character(x) && is.vector(x),
				profile$character_vector),

			list(
				is.matrix,
				profile$matrix )
		)

		for (pair in response_pairs) {

			test <- pair[[1]]
			response <- pair[[2]]

			if (test(obj)) {
				return (response(obj))
			}
		}

	profile$default(obj)
	}
})





