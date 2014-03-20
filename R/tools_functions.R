
# Package Internals
#
# Documentation for the internals of Arrow.
# Every sufficiently large utility library will contain its own utility library.
# These functions are required to reduce repetition of code when implementing arrow
# functions.
#
# @keywords internal
# @rdname pkg-internal

# --------------------- shorthand logical functions --------------------- #

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

# @section one_of:
#
# Return one value from a collection.
#
# @keywords internal
# @rdname pkg-internal

one_of <- function (coll) {
	# coll [any] -> any
	# select a single value from a collection.

	ith <- sample(seq_along(coll), size = 1)
	coll[[ith]]
}

# @section equals:
#
# R's equal operator doesn't work on lists or strange values.
# equals is a better measure of identity.
#
# @keywords internal
# @rdname pkg-internal

'%equals%' <- function (a, b) {
	# are two values identical?

	identical(a, b)
}

# @section call_with_params:
#
# Construct a call to a function 'fnname' with the parametres of
# a second function. Useful for higher order functions.
#
# @keywords internal
# @rdname pkg-internal

call_with_params <- function (fnname, fn) {
	# string -> function -> call
	# create call for a function with
	# the arguments of another function.

	as.call(
		lapply(
			c(fnname, names(xFormalsOf(fn)) ),
			as.symbol))
}

# @section +:
#
# Concatenate two strings.
#
# @keywords internal
# @rdname pkg-internal

"%+%" <- function (x, y) {
	# javascript-style string concatenation.

	paste0(x, y, sep = "")
}

# @section in:
#
# An infix function to test for the non-membership of an element in a set.
#
# @keywords internal
# @rdname pkg-internal

'%!in%' <- function (x, y) {
	!(x %in% y)
}

# to dedottify my code.
match_fn <- match.fun

# --------------------- environment manipulation --------------------- #

# @section Object:
#
# Construct an empty environment.
#
# @keywords internal
# @rdname pkg-internal

Object <- function () {
	# construct an empty environment.

	new.env(parent = emptyenv())
}

# @section join_env:
#
# Join two environments together into one environment. This
# allows for inheritance of environments without having
# to traverse multiple environments.
#
# @keywords internal
# @rdname pkg-internal

join_env <- function (x, y) {
	# do not use this often; it's a very slow
	# way of joining two environments.

	if (missing(x)) {
		stop("internal error: joining environments failed.")
	}
	if (missing(y)) {
		stop("internal error: joining environments failed.")
	}

	as.environment( c(as.list( x ), as.list( y )) )
}

# --------------------- property tests --------------------- #

# @section is_fn_matchable:
#
# Is a value a function, or possibly the name of a function.
#
# @keywords internal
# @rdname pkg-internal

is_fn_matchable <- function (val) {
	# is a value a function or matchable as a function?

	is.function(val) || is.symbol(val) ||
	(is.character(val) && length(val) == 1)
}

# @section is_collection:
#
# Is a value a generic or atomic vector or a pairlist.
#
# @keywords internal
# @rdname pkg-internal


is_collection <- function (val) {
	# is a value a pairlist, list or typed vector?

	# don't change - is.vector doesn't handle attributes.
	is.atomic(val) || is.list(val) || is.pairlist(val)
}

is_recursive <- function (val) {
	is.list(val) || is.pairlist(val)
}

maybe_atomic <- function (val) {
	length(val) %in% 0:1
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

try_hof <- function (expr, invoking_call) {
	# expression -> call -> any
	# provide a good error message if a higher-order function
	# fails because the user provided a bad function.

	tryCatch(
		expr,
		warning = function (warn) {

			apically_calling_fn <- invoking_call[[1]]

			assert(
				False, invoking_call,
				yelp$warning_higher_order(apically_calling_fn, warn))

		},
		error = function (err) {

			apically_calling_fn <- invoking_call[[1]]

			assert(
				False, invoking_call,
				yelp$error_higher_order(apically_calling_fn, err))

		}
	)
}

try_write <- local({
	function (expr, path, invoking_call) {

		tryCatch(
			expr,
			warning = function (warn) {
				apically_calling_fn <- invoking_call[[1]]

				write_warning(
					yelp$arrow_function_failed(
						components$invoking, components$calltext, message),
					call. = False)
			},
			error = function (err) {
				apically_calling_fn <- invoking_call[[1]]

				write_error(
					yelp$arrow_function_failed(
						components$invoking, components$calltext, message),
					call. = False)
			}
		)
	}

})

try_read <- local({
	function (expr, path, invoking_call) {

		tryCatch(
			expr,
			warning = function (warn) {
				apically_calling_fn <- invoking_call[[1]]

				assert(
					False, invoking_call,
					yelp$warning_read(path, warn)
				)
			},
			error = function (err) {
				apically_calling_fn <- invoking_call[[1]]

				assert(
					False, invoking_call,
					yelp$error_read(path, err)
				)
			}
		)
	}

})

# --------------------- testing & message functions --------------------- #


ddparse <- function (val, collapse = "") {
	# safely deparse a string.

	paste0(deparse(val), collapse = collapse)
}

ddquote <- function (sym) {
	paste0(dQuote(sym), collapse = '')
}

newline <- function (val) {
	paste0(val, collapse = "\n")
}

wrap <- function (...) {
	# wrap and indent a string,

	paste0(
		strwrap(...),
		collapse = '')
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

summate <- local({
	# Returns a string of information about an input object.

	output_key_value_pairs <-
		function (coll) {

			Reduce(
				'%+%',
				lapply(names(coll), function (name) {

					name %+% ':\n' %+%
					'    ' %+% paste0(coll[[name]], collapse = '') %+% '\n'

			}) )
		}

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

			"\n\n" %+% "[ properties of the error-causing character vector ]:" %+% "\n\n" %+%
			output_key_value_pairs(traits)

		}

	profile$closure <-
		function (obj) {

			traits <- list(
				is_primitive =
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

			"\n\n" %+% "[ properties of the error-causing function ]:" %+% "\n\n" %+%
			output_key_value_pairs(traits)

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

			"\n\n" %+% "[ properties of the error-causing value ]:" %+% "\n\n" %+%
			output_key_value_pairs(traits)

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

			"\n\n" %+% "[ properties of the error-causing double vector ]:" %+% "\n\n" %+%
			output_key_value_pairs(traits)

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

			"\n\n" %+% "[ properties of the error-causing factor ]:" %+% "\n\n" %+%
			output_key_value_pairs(traits)

		}

	# --- G --- #

	profile$generic_vector <-
		function (obj) {

			traits <- list(
				length =
					length(obj)
			)

			"\n\n" %+% "[ properties of the error-causing list or pairlist ]:" %+% "\n\n" %+%
			output_key_value_pairs(traits)

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

			"\n\n" %+% "[ properties of the error-causing integer vector ]:" %+% "\n\n" %+%
			output_key_value_pairs(traits)

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


			"\n\n" %+% "[ properties of the error-causing logical vector ]:" %+% "\n\n" %+%
			output_key_value_pairs(traits)

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

			"\n\n" %+% "[ properties of the error-causing matrix ]:" %+% "\n\n" %+%
			output_key_value_pairs(traits)

		}

	# --- N --- #

	profile$null <-
		function (obj) {

			"\n\n" %+% "[ properties of ther error-causing object ]:" %+% "\n\n" %+%
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


			"\n\n" %+% "[ properties of the error-causing raw vector ]:" %+% "\n\n" %+%
			output_key_value_pairs(traits)

		}

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

			"\n\n" %+% "[ properties of the error-causing character vector ]:" %+% "\n\n" %+%
			output_key_value_pairs(traits)

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

			"\n\n" %+% "[ properties of the error-causing character vector ]:" %+% "\n\n" %+%
			output_key_value_pairs(traits)

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
				function (x) is.logical(x),
				profile$logical_vector),
			list(
				function (x) is.raw(x),
				profile$raw_vector),
			list(
				function (x) is.integer(x),
				profile$integer_vector),
			list(
				function (x) is.double(x),
				profile$double_vector),
			list(
				function (x) is.character(x),
				profile$character_vector),

			list(
				is.matrix,
				profile$matrix )
		)

		for (pair in response_pairs) {

			test <- pair[[1]]
			response <- pair[[2]]

			if (test(obj)) {
				message <- paste0(response(obj), collapse = '')

				stopifnot(length(message) == 1)
				return (message)
			}
		}

	profile$default(obj)
	}
})
