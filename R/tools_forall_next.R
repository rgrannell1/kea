
# -------------------------------- from_stream -------------------------------- #
#
# from_stream emits random values.
#
#
#
#

from_stream <- function (...) {
	# -- yield a single valid R object.

	#-- finish this alphabet
	extended_ascii <- strsplit("abcdefghijklmnopqrsruvwxyz0123456789", '')[[1]]
	# -- needed, as strsplit can do weird things.
	extended_ascii <- Filter(function (x) length(x) > 0, extended_ascii)

	# -- whitespace
	whitespace <- strsplit(" 	", '')[[1]]
	whitespace <- Filter(function (x) length(x) > 0, whitespace)

	this <- Object()

	# -- character vectors

	this $ empty_character <-
		function () {
			character(0)
		}

	this $ character <-
		function (...) {
			one_of(extended_ascii)
		}

	this $ word <-
		function (...) {
			no_chars <- abs( rnorm(1, 1, sd = length(extended_ascii)) )

			paste0(
				rsample(extended_ascii, size = no_chars, replace = True),
				collapse = '')
		}

	this $ line <-
		function (...) {

			no_words <- abs( rnorm(1, 1, sd = 100) )

			words <- unlist(lapply(seq_len(no_words), this $ word))

			paste0(words, collapse = rsample(whitespace, size = 1))
	}

	this $ paragraph <-
		function (...) {
			no_lines <- abs( rnorm(1, 1, sd = 10) )

			lines <- unlist(lapply(seq_len(no_lines), this $ line))

			paste0(lines, collapse = '\n')
		}

	# -- logical vectors

	this $ empty_logical <-
		function () {
			logical(0)
		}

	this $ flag <-
		function () {
			one_of(c(True, False, Na))
		}

	this $ logicals <-
		function () {
			no_bools <- abs(rnorm(1, 1, sd = 100))

			rsample(c(True, False, Na), size = no_bools, replace = True)
		}

	# -- symbols
#	this $ symbol <-
#		function () {

#			word <- this $ word()

#			while (nchar(word) == 0) {
#				word <- this $ word()
#			}

#			as.symbol(word)
#		}

	# -- double

	this $ empty_double <-
		function () {
			numeric(0)
		}
	this $ nan <-
		function () {
			NaN
		}

	this $ infinity <-
		function () {
			one_of(c(-Inf, +Inf))
		}

	this $ double <-
		function () {
			rnorm(1, 0, 1000000)
		}

	# -- integer

	this $ empty_integer <-
		function () {
			integer(0)
		}

	this $ integer <-
		function () {
			sample.int(2147483647, 1) * rsample(c(-1, 1), size = 1)
		}

	# -- function

	this $ base <-
		local({
			base <- Filter(is.function, lapply(ls('package:base'), get))

			function () {
				one_of(base)
			}
		})

	# -- with that out of the way, yield a value.

	implemented <- ls(envir = this)

	sampler <- this[[ rsample(implemented, size = 1) ]]
	sampler()
}




























# validate test takes the test object generated with the
# forall language functions, and modifies and checks it
# for use in execute_test.

validate_test <- function (invoking_call, test) {

	# -- make sure positives or negatives is set.
	if (is.null(test $ positives) && is.null(test $ negatives)) {
		message <-
			'either positives or negatives must be set.'

		throw_kiwi_error(invoking_call, message)

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

			throw_kiwi_error(invoking_call, message)
		}
	}

	if (!is.numeric(test $ time) || length(test $ time) < 0) {
		message <-
			'time must be a positive number'

		throw_kiwi_error(invoking_call, message)
	}

	if (length(test $ info) != length(test $ positives) + length(test $ negatives)) {
		message <-
			'there must be a one and only one description for each positive or negative test.'

		throw_kiwi_error(invoking_call, message)
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

	for (group_ith in seq_along(groups)) {

		group       <- groups[[group_ith]]
		group_pred  <- group[[1]]
		group_props <- group[2:length(group)]

		is_match <- tryDefault(do.call(group_pred, case), False)

		state $ case_examined <- state $ case_examined + 1

		if (!isTRUE(is_match)) {
			next
		}

		state $ tests_run <- state $ tests_run + 1

		for (prop_ith in seq_along(group_props)) {

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

yield_case <- function (params) {
	lapply(seq_along(params), from_stream)
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










positive_test <- function (prop, case) {
	do.call(prop, case)
}

negative_test <- function (prop, case) {
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

	initial_state <- function () {
		list(
			cases_examined = 0,
			tests_run      = 0,
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
		positive = initial_state(),
		negative = initial_state()
	)

	# -- testers take a property, and a test case, and return a boolean value.
	testers <- list(
		positive_test,
		negative_test
	)

	# -- test random test cases for a preset amount of time.

	time_left <- xStopwatch(time)

	while (time_left()) {

		# -- generate a random test case.

		case   <- yield_case(params)
		states <- Map(
			function (test, group, state) {

				run_test(test, group, state, case, info, invoking_call)

			},
			testers,
			group_types,
			states
		)
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
#
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

# -- TODO

suchThat <- function (...) {

	invoking_call <- sys.call()

	args    <- as.list(match.call()[-1])
	pred    <- args[[ length(args) ]]
	symbols <- args[1:(length(args)-1)]

	if (missing(..1)) {
		message <-
			'suchThat must provide a symbol to select over.'

		throw_kiwi_error(invoking_call, message)
	}

	out <- list(



	)

	class(out) <- c('xforall', ' xsuchthat')
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

		throw_kiwi_error(invoking_call, message)
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

		throw_kiwi_error(invoking_call, message)
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

		throw_kiwi_error(invoking_call, message)
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
