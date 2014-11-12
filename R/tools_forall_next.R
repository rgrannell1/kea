
# -------------------------------- predicate composition -------------------------------- #
#
# Much of the work in writing tests is selecting test cases. These predicates should be composed
# to reduce repetition.

not <- function (pred) {
	function (x) {
		!pred(x)
	}
}



and <- function (preds) {
	function (x) {

		all( vapply(preds, function (pred) {
			pred(x)
		}, logical(1)) )

	}
}

and_ <- function (...) {
	and(list(...))
}



or <- function (preds) {
	function (x) {

		any( vapply(preds, function (pred) {
			pred(x)
		}, logical(1)) )

	}
}

or_ <- function (...) {
	or(list(...))
}





suchThat <- ( function () {

	this <- list( )

	this $ is_collection        <- is_collection
	this $ is_empty_collection  <- and_(is_collection, function (x) length(x) == 0)

	this $ not_collection       <- not(this $ is_collection)
	this $ not_empty_collection <- not(this $ is_empty_collection)

	this $ is_named_collection  <- and_(this $ is_collection, this $ is_named)
	this $ not_named_collection <- and_(this $ is_collection, this $ not_named)

	this $ is_named             <- is_named
	this $ not_named            <- not(this $ is_named)

	this $ is_logical           <- is_logical
	this $ is_character         <- is_character

	this $ is_function          <- is.function
	this $ is_primitive         <- is.primitive
	this $ is_pairlist          <- or_(is.pairlist, is.null)

	this $ is_closure           <- and_(this $ is_function, not(this $ is_primitive))
	this $ not_closure          <- not(this $ is_closure)

	this $ is_true              <- isTRUE
	this $ not_true             <- not(this $ is_true)

	this $ is_false             <- function (x) identical(x, FALSE)
	this $ not_false            <- not(this $ is_false)

	this $ is_null              <- function (x) identical(x, NULL)
	this $ not_null             <- not(this $ is_null)

	this $ is_nan               <- is_nan
	this $ not_nan              <- not(is_nan)

	this $ is_na                <- is_na
	this $ not_na               <- not(is_na)


	this

} )()






# -------------------------------- from_stream -------------------------------- #
#
# from_stream emits random values.
#
#
#
#

vectorise <- function (atom, type) {

	types <- list(
		integer   = integer(1),
		double    = double(1),
		numeric   = numeric(1),
		logical   = logical(1),
		character = character(1),
		raw       = raw(1)
	)

	example <- types[[type]]

	function (len) {
		vapply(seq_len(len), function (ith) {
			len <- sample.int(abs( round(rnorm(1, 0, 20)) ) + 1, size = 1)
			atom(len)
		}, example )
	}
}






pick_one <- function (coll) {
	function (len) {
		one_of(coll)
	}
}

pick_one_ <- function (...) {
	pick_one(list(...))
}






one_gen  <- function (fns) {
	function (len) {
		one_of(fns)(len)
	}
}

one_gen_ <- function (...) {
	one_gen(list(...))
}

listify <- function (atom) {
	function (len) {
		as.list(atom(len))
	}
}

rlistify <- function (atoms) {
	function (len) {

		lapply(seq_len(length(atoms)), function (.) {
			atom <- one_of(atoms)
			atom(max(len - 1, 0))

		})
	}
}








from_stream <- ( function () {
	# -- yield a single valid R object.

	#-- finish this alphabet
	ascii <- strsplit( "!\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~", '')[[1]]
	# -- needed, as strsplit can do weird things.
	ascii <- Filter(function (x) length(x) > 0, ascii)

	# -- whitespace
	whitespace <- c(' ', '	')

	this <- list()

	# -- na's

	this $ na <-
		pick_one_(NA, NA_integer_, NA_real_, NA_character_, NA_complex_)

	# -- character vectors

	this $ empty_character <-
		pick_one_(character(0))

	this $ character <-
		pick_one(ascii)

	this $ word <-
		function (len) {

			paste0(
				rsample(ascii, size = len, replace = True),
				collapse = '')
		}

	this $ line <-
		function (len) {

			words <- vapply(seq_len(len), function (ith) {
				this $ word(sample.int(abs( round(rnorm(1, 0, 20)) ) + 1, size = 1))
			}, character(1))

			paste0(words, collapse = rsample(whitespace, size = 1))
	}

	this $ paragraph <-
		function (len) {

			lines <- unlist(lapply(seq_len(len), this $ line))

			paste0(lines, collapse = '\n')
		}

	this $ characters <-
		vectorise(this $ character, 'character')

	this $ words <-
		vectorise(this $ word, 'character')

	this $ lines <-
		vectorise(this $ line, 'character')

	this $ paragraphs <-
		vectorise(this $ paragraph, 'character')

	add_names <- function (fn) {
		function (len) {
			elems        <- fn(len)

			if (length(elems) == 0) {
				structure(elems, names = character(0))
			} else {

				names(elems) <- vapply(elems, function (elem) {
					this $ word(sample.int(abs( round(rnorm(1, 0, 20)) ) + 1, size = 1))
				}, character(1))

				elems
			}
		}
	}

	# -- logical vectors

	this $ empty_logical <-
		pick_one_(logical(0))

	this $ logical <-
		pick_one_(True, False, Na)

	this $ logicals <-
		vectorise(this $ logical, 'logical')

	# -- double

	this $ empty_double <-
		pick_one_(numeric(0))

	this $ nan <-
		pick_one_(NaN)

	this $ nans <-
		vectorise(this $ nan, 'numeric')

	this $ infinity <-
		pick_one_(-Inf, +Inf)

	this $ infinities <-
		vectorise(this $ infinity, 'numeric')

	this $ double <-
		function (len) {
			rnorm(1, 0, 1000000)
		}

	this $ doubles <-
		vectorise(this $ double, 'numeric')

	this $ doubles_any <-
		one_gen_(this $ double, this $ infinity, this $ nan, this $ na)






	# -- integer

	this $ empty_integer <-
		pick_one_(integer(0))

	this $ integer <-
		function (len) {
			sample.int(2147483647, 1) * rsample(c(-1L, 1L), size = 1)
		}

	this $ integers <-
		vectorise(this $ integer, 'integer')

	this $ integers_any <-
		one_gen_(this $ integer, this $ na)





	# -- factors



	# -- named vector

	this $ named_doubles_any <-
		add_names(this $ doubles_any)

	this $ named_integers_any <-
		add_names(this $ integers_any)

	this $ named_empty_character <-
		add_names(this $ empty_character)

	this $ named_empty_logical <-
		add_names(this $ empty_logical)

	this $ named_empty_double <-
		add_names(this $ empty_double)

	this $ named_empty_integer <-
		add_names(this $ empty_integer)

	#  -- typed generic lists

	this $ na_list <-
		listify(this $ na)

	this $ character_list <-
		listify(this $ character)

	this $ word_list <-
		listify(this $ word)

	this $ line_list <-
		listify(this $ line)

	this $ paragraph_list <-
		listify(this $ paragraph)

	this $ logical_list <-
		listify(this $ logical)

	this $ logicals_list <-
		listify(this $ logicals)

	this $ nan_list <-
		listify(this $ nan)

	this $ nans_list <-
		listify(this $ nans)

	this $ infinity_list <-
		listify(this $ infinity)

	this $ infinities_list <-
		listify(this $ infinities)

	this $ double_list <-
		listify(this $ double)

	this $ doubles_list <-
		listify(this $ doubles)

	this $ doubles_any_list <-
		listify(this $ doubles_any)

	this $ integer_list <-
		listify(this $ integer)

	this $ integers_list <-
		listify(this $ integers)

	this $ integers_any_list <-
		listify(this $ integers_any)

	#  -- named typed generic lists

	this $ named_na_list <-
		add_names(listify(this $ na))

	this $ named_character_list <-
		add_names(listify(this $ character))

	this $ named_word_list <-
		add_names(listify(this $ word))

	this $ named_line_list <-
		add_names(listify(this $ line))

	this $ named_paragraph_list <-
		add_names(listify(this $ paragraph))

	this $ named_logical_list <-
		add_names(listify(this $ logical))

	this $ named_logicals_list <-
		add_names(listify(this $ logicals))

	this $ named_nan_list <-
		add_names(listify(this $ nan))

	this $ named_nans_list <-
		add_names(listify(this $ nans))

	this $ named_infinity_list <-
		add_names(listify(this $ infinity))

	this $ named_infinities_list <-
		add_names(listify(this $ infinities))

	this $ named_double_list <-
		add_names(listify(this $ double))

	this $ named_doubles_list <-
		add_names(listify(this $ doubles))

	this $ named_doubles_any_list <-
		add_names(listify(this $ doubles_any))

	this $ named_integer_list <-
		add_names(listify(this $ integer))

	this $ named_integers_list <-
		add_names(listify(this $ integers))

	this $ named_integers_any_list <-
		add_names(listify(this $ integers_any))

	# -- function

	this $ base <-
		local({
			base <- Filter(is.function, lapply(ls('package:base'), get))

			function (len) {
				one_of(base)
			}
		})

	# -- with that out of the way, yield a value.

	function (len) {

		implemented <- names(this)
		sampler     <- this[[ rsample(implemented, size = 1) ]]

		sampler(len)
	}

})()




























# validate test takes the test object generated with the
# forall language functions, and modifies and checks it
# for use in execute_test.

validate_test <- function (invoking_call, test) {

	# -- make sure positives or negatives is set.
	if (is.null(test $ positives) && is.null(test $ negatives)) {
		message <-
			'either positives or negatives must be set.'

		throw_exception $ error(invoking_call, message)

	} else if (is.null(test $ positives)) {
		test $ positives <- list()
	} else if (is.null(test $ negatives)) {
		test $ negatives <- list()
	}

	# -- throw an error if any test fields are missing.
	for (key in c('info', 'params', 'time')) {
		if (is.null( test[[key]] )) {
			message <-
				'the property ' %+% key %+% ' is missing from the test object.'

			throw_exception $ error(invoking_call, message)
		}
	}

	if (!is.numeric(test $ time) || length(test $ time) < 0) {
		message <-
			'time must be a positive number'

		throw_exception $ error(invoking_call, message)
	}

	if (length(test $ info) != length(test $ positives) + length(test $ negatives)) {
		message <-
			'there must be a one and only one description for each positive or negative test.'

		throw_exception $ error(invoking_call, message)
	}

	test $ info <- dQuote(test $ info)

	test
}







# Parameterise takes a list of expression-lists, and
# adds parametres to each expression, and attaches the
# correct environment.

parameterise <- function (exprgroups, params, envir) {

	lapply(exprgroups, function (exprs) {
		lapply(exprs, function (expr) {

			shell <- function () {}

			# -- add the parameters given by over, and use the parent env.
			body(shell) <- expr
			environment(shell) <- envir
			formals(shell) <- as_formals(params)

			shell
		})
	})
}










run_test <- function (tester, groups, state, case, info, invoking_call) {
	# -- there can be several holdsWhen(pred, exp1, exp2) groups.

	for ( group_ith in seq_len(length(groups)) ) {

		group       <- groups[[group_ith]]
		group_pred  <- group[[1]]
		group_props <- group[2:length(group)]

		# -- warn for errors or warning, as these are often symptoms
		# -- of an improperly written test.
		is_match <- tryCatch(
			do.call(group_pred, case),
			warning = function (warn) {
				warning(warn)
				False
			},
			error   = function (err)  {
				warning(err)
				False
			}
		)

		state $ cases_examined[[group_ith]] <- state $ cases_examined[[group_ith]] + 1

		if (!identical(is_match, TRUE)) {
			next
		}

		state $ tests_run[[group_ith]] <- state $ tests_run[[group_ith]] + 1

		for ( prop_ith in seq_len(length(group_props)) ) {

			prop     <- group_props[[prop_ith]]
			has_prop <- tester(prop, case)

			# -- to aid debugging.
			if (length(has_prop) != 1) {

				message <-
					info %+% '\n' %+%
					colourise $ red('Failed!') %+%
					'the property ' %+% ddparse(body(prop)) %+%
					' returned a non length-one result\n' %+%
					'For the test case ' %+% ddparse(case)

				stop(message, call. = False)
			}

			# -- the result of the property must always be true or false.
			if (!isTRUE(has_prop) && !identical(has_prop, False)) {

				message <-
					info %+% '\n' %+%
					colourise $ red('Failed! ') %+%
					'the property ' %+% ddparse(body(prop)) %+%
					' returned a non-logical result\n' %+%
					'For the test case ' %+% ddparse(case)

				stop(message, call. = False)
			}

			if (!isTRUE(has_prop)) {

				# -- make sure the earliest failure is noted.
				state $ failed_after <-
					min(state $ failed_after, state $ tests_run)

				# -- store the failed case.
				state $ fails_for <-
					c(state $ fails_for, list(case))

				# -- store the index of the failed properties.
				failed_index <- list(
					group    = group_ith,
					property = prop_ith,
					summary  = paste(group_ith, ',', prop_ith)
				)

				state $ failed_indices <-
					c(state $ failed_indices, list(failed_index))
			}
		}
	}
	state
}










# Generate a random test case for each parametre we decided to
# test over.

yield_case <- function (params, len) {
	lapply(seq_len(length(params)), function (...) from_stream(len))
}







# Positive controls failed.
#


throw_exhaustion_warning <- function (test_data, state, info, invoking_call) {

	run      <- state $ tests_run
	examined <- state $ case_examined

	message <- info %+% colourise $ yellow(" Failed!\n") %+%
	"all " %+% examined %+% " test cases" %+%
		" were rejected."

	warning(message, call. = False)
}











throw_property_error <- function (test_data, state, invoking_call) {

	after      <- state $ failed_after
	fails_for  <- state $ fails_for

	# -- remove names for readability.

	case_string <- vapply(lapply(fails_for, unname), ddparse, character(1))[[1]]

	indices   <- state $ failed_indices
	num_fails <- as.numeric(as.list( table(sapply( indices, function (triple) {
		triple $ summary
	} )) ))

	# -- which_failed, expressions, and descriptions are co-ordered

	which_failed <- unique(indices)

	expressions  <- lapply(which_failed, function (triple) {

		ith <- triple $ group
		# -- to account for the predicate at the head.
		jth <- triple $ property + 1

		test_data $ properties[[ith]][[jth]]
	})

	descriptions <- lapply(which_failed, function (triple) {
		test_data $ info[[ triple $ group ]]
	})

	paragraphs <- Map(
		function (freq, expr, info) {

			# -- info is already double quoted.
			paste0(
				info, '\n',
				'the assertion "', ddparse(expr), '" failed ', freq, ' times.')

		},
		num_fails,
		expressions,
		descriptions
	)

	summary <- paste(paragraphs, collapse = '\n\n')

	message <- colourise $ red("\nFailed ") %+%
	"after the " %+%
		ith_suffix(after) %+% " case!\n\n" %+%
		summary %+% "\n\n" %+%
		case_string %+% "\n"

	stop(message, call. = False)
}








# Everything worked. Report the fact.

state_sucess <- function (states, info) {
	# -- report that the tests all passed.

	positive_run <- states [[1]] $ tests_run
	negative_run <- states [[2]] $ tests_run

	if (length(negative_run) == 0) {
		negative_run <- rep(0, length(positive_run))
	} else 	if (length(positive_run) == 0) {
		positive_run <- rep(0, length(negative_run))
	}




	# -- info is vectorised (many descriptions), create newline for each.

	run_summary <-
		"(" %+%
			sprintf("%-8s",colourise $ green(positive_run %+% '+') %+% ',') %+%
			colourise $ red  (negative_run %+%"-") %+%
		')'

	msg <- paste0(
		sprintf("%-80s", info %+% " passed! ") %+% run_summary, collapse = '\n')

	message(msg)

}










holdswhen_test <- function (prop, case) {
	do.call(prop, case)
}

failswhen_test <- function (prop, case) {
	# -- return false if the test doesn't throw an error. Otherwise
	# -- return true.
	tryDefault(
		{
			do.call(prop, case)
			False
		},
		True
	)
}










# The backend for the test. Takes a forall object,
# and executes the encoded test.
#
# The terms positive and negative (as in controls) are loosely
# related to the scientific usage.
#
# Positive controls are expected to return true, that is
# they verify that the function has the expected property.
#
# Negative controls are expected to not work (this is the looser
# of the two terms), in that it throws an error.

execute_test <- function (test) {

	invoking_call <- sys.call()
	parent_frame <- parent.frame()

	test <- validate_test(invoking_call, test)

	# -- attach the test object to the local environment.
	info       <- test $ info
	params     <- test $ params
	positives  <- test $ positives
	negatives  <- test $ negatives
	time       <- test $ time

	# -- the state that is modified after running several tests.
	# -- failed_indices needed to identify failing expression & associated description.

	initial_state <- function (num) {
		list(
			cases_examined = rep(0, num),
			tests_run      = rep(0, num),
			fails_for      = list(),
			failed_after   = Inf,
			failed_indices = list()
		)
	}

	# -- add parametres to each failure and property.
	group_types <- Map(
		function (group) {
			parameterise(group, params, parent_frame)
		},
		list(positives, negatives)
	)

	states <- list(
		positive = initial_state(length(positives)),
		negative = initial_state(length(negatives))
	)

	# -- testers take a property, and a test case, and return a boolean value.
	testers <- list(
		holdswhen_test,
		failswhen_test
	)

	# -- test random test cases for a preset amount of time.

	len       <- 1
	time_left <- xStopwatch(time)

	while (time_left()) {

		# -- generate a random test case.

		case   <- yield_case(params, len)

		states <- Map(
			function (test, group, state) {

				run_test(test, group, state, case, info, invoking_call)

			},
			testers,
			group_types,
			states
		)

		len <- len + rsample(0:1, size = 1, prob = c(0.9, 0.1))
	}

	test_data <- list(info = info, time = time)

	# -- check that the correct number of tests were run, and that no tests failed.
	Map(
		function (group, state) {

			if (length(group) > 0 && state $ tests_run == 0) {
				throw_exhaustion_warning(test_data, state, info, invoking_call)
			}

			if (length(state $ fails_for) > 0) {
				# -- throw errors for +, - if required.
				throw_property_error(
					add_field(test_data, 'properties', group), state, invoking_call)
			}

		},
		list(positives, negatives),
		states
	)

	state_sucess(states, info)
	invisible(Null)
}










# -------------------------------- Grammar -------------------------------- #

# describe(str)                 add a description to a property group. Printed
#                                on error.
#
# over(...symbols)              give the parametres to be bound to random values
#                                (singleton field).
#
# holdsWhen(expr, ...expr)      when a predicate is true of the randomly generated test cases,
#                               check that several properties are also true.
#
# failsWhen(expr, ...expr)      when a predicate is true of the randomly generated test cases,
#                               check that several functions fail.
#
# worksWhen(expr, ...expr)      when a predicate is true of the randomly generated test cases,
#                               check that several functions run correctly.
#
#
# run(num)                      execute the unit test object, set the time to execute for.
#
# chains with +





# -- the description
#
#

describe <- function (info) {
	out <- list(info = info)
	class(out) <- c('xforall', 'xdescribe')
	out
}





# -- the domain over which to bind
#
#

over <- function (...) {

	# -- capture the symbols
	symbols <- as.list(match.call()[-1])

	# -- validate the symbols

	stopifnot( vapply(symbols, is.name, logical(1)) )

	params <- vapply(symbols, paste, character(1))

	out <- list(params = params)
	class(out) <- c('xforall', 'xover')
	out
}

# -- test positives (+ controls)
#
#

holdsWhen <- function (expr1, ...) {

	invoking_call <- sys.call()

	exprs <- as.list(match.call(expand.dots = False)[-1])

	if (missing(..1)) {
		message <-
			'holdsWhen must specify expectations.'

		throw_exception $ error(invoking_call, message)
	}

	out <- list(
		positives =
			c(list( exprs[[1]] ), exprs$...)
	)
	class(out) <- c('xforall', 'xholdswhen')
	out

}

#
#
# This is a hack - should be implemented as a first-class
# property.

worksWhen <- function (expr1, ...) {

	invoking_call <- sys.call()

	exprs <- as.list(match.call(expand.dots = False)[-1])

	if (missing(..1)) {
		message <-
			'worksWhen must specify expectations.'

		throw_exception $ error(invoking_call, message)
	}

	# if the expression runs, return tre.
	exprs$... <- lapply(exprs$..., function (expr) {

		join_exprs(expr, {TRUE})

	})

	out <- list(
		positives =
			c(list( exprs[[1]] ), exprs$...)
	)
	class(out) <- c('xforall', 'xholdswhen')
	out

}




# -- test failures (- controls)
#
#

failsWhen <- function (expr1, ...) {

	invoking_call <- sys.call()

	exprs <- as.list(match.call(expand.dots = False)[-1])
	if (missing(..1)) {
		message <-
			'failsWhen must specify expectations.'

		throw_exception $ error(invoking_call, message)
	}

	out <- list(
		negatives =
			c(list( exprs[[1]] ), exprs$...)
	)
	class(out) <- c('xforall', 'xfailsWhen')
	out

}






# Run specifies that the test object should now be
# executes. Also specifies how long to run the test for.

run <- function (time = 2) {
	out <- list(time = time)
	class(out) <- c('xforall', 'xrun')
	out
}








# -------------------------------- + -------------------------------- #
#
# The + operator joins two forall objects into a new forall object.
# Non-associative, list concatenation that either overrides or joins
# shared keys.

'+.xforall' <- function (acc, new) {
	# -- an operation that joins any
	# -- member of 'xforall' into a compound object,
	# -- unless run is joined.
	# -- run signals execution.

	override <- function (key) {
		# -- set or override a key the accumulator object.
		# -- optionally execute a callback on the accumulator.

		function () {
			acc[[key]] <- new[[key]]
			acc
		}
	}

	join <- function (key) {
		# -- concatenate the new value at a key to the old value.
		# -- optionally execute a callback on the accumulator.

		function () {
			if (length(acc[[key]]) > 0) {
				acc[[key]] <- c(acc[[key]], list(new[[key]]))
			} else {
				acc[[key]] <- list(new[[key]])
			}
			acc
		}
	}

	responses <- list(
		'xdescribe'  = join('info'),
		'xover'      = override('params'),
		'xholdswhen' = join('positives'),
		'xfailsWhen' = join('negatives'),
		'xrun'       = function () {
			acc $ time <- new $ time

			execute_test(acc)
			invisible(Null)
		}
	)

	new_classes <- class(new)

	for (classname in names(responses)) {
		if (any(new_classes == classname)) {
			return ( responses[[classname]]() )
		}
	}

	invisible(Null)
}






# One-Off testing functions.
#

holdsFor <- function (info, ...) {

	invoking_call <- sys.call()

	if (!is.character(info)) {

		throw_exception $ type_error(invoking_call, "description missing or invalid.")

	}

	lapply(match.call(expand.dots = FALSE) $ ..., function (assertion) {

		passed <- tryCatch(
			eval(assertion),
			error = function (err) {

				message <-
					info %+% '\n' %+%
					colourise $ red('Failed! ') %+%
					'the property ' %+% ddparse(assertion) %+%
					' threw an error.'

				throw_exception $ error(invoking_call, message)

			}
		)

		if (!is.logical(passed)) {

			message <-
				info %+% '\n' %+%
				colourise $ red('Failed! ') %+%
				'the property ' %+% ddparse(assertion) %+%
				' returned a non-logical value.'

			throw_exception $ type_error(invoking_call, message)

		}

		if (length(passed) != 1) {

			message <-
				info %+% '\n' %+%
				colourise $ red('Failed! ') %+%
				'the property ' %+% ddparse(assertion) %+%
				' returned a non-length one value.'

			throw_exception $ value_error(invoking_call, message)

		}

		if (!isTRUE(passed)) {

			message <-
				info %+% '\n' %+%
				colourise $ red('Failed! ') %+%
				'the property ' %+% ddparse(assertion) %+%
				' was false.'

			throw_exception $ value_error(invoking_call, message)

		}

	})

	invisible(NULL)
}





worksFor <- function (info, ...) {

	invoking_call <- sys.call()

	if (!is.character(info)) {

		throw_exception $ type_error(invoking_call, "description missing or invalid.")

	}

	lapply(match.call(expand.dots = FALSE) $ ..., function (assertion) {

		tryCatch(
			eval(assertion),
			error = function (err) {

				message <-
					info %+% '\n' %+%
					colourise $ red('Failed! ') %+%
					'the property ' %+% ddparse(assertion) %+%
					' threw an error.'

				throw_exception $ error(invoking_call, message)

			}
		)

	})

	invisible(NULL)
}










failsFor <- function (info, ...) {

	invoking_call <- sys.call()

	if (!is.character(info)) {

		throw_exception $ type_error(invoking_call, "description missing or invalid.")

	}

	lapply(match.call(expand.dots = FALSE) $ ..., function (assertion) {

		fails <- tryCatch(
			{
				eval(assertion)
				FALSE
			},
			error = function (err) {TRUE}
		)

		if (!fails) {

			message <-
				info %+% '\n' %+%
				colourise $ red('Failed! ') %+%
				'the property ' %+% ddparse(assertion) %+%
				' did not throw an error.'

			throw_exception $ error(invoking_call, message)

		}

	})

	invisible(NULL)
}
