
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
	this $ symbol <-
		function () {

			word <- this $ word()

			while (nchar(word) == 0) {
				word <- this $ word()
			}

			as.symbol(word)
		}

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

		throw_arrow_error(invoking_call, message)

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

			throw_arrow_error(invoking_call, message)
		}
	}

	# -- validate the test fields
	if (!is.character(test $ info) || length(test $ info) != 1) {
		message <-
			'the property info must be a length-one character vector.'

		throw_arrow_error(invoking_call, message)
	}

	if (!is.numeric(test $ time) || length(test $ time) < 0) {
		message <-
			'time must be a positive number'

		throw_arrow_error(invoking_call, message)
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
			formals(shell) <- make_formals(params)

			shell
		})
	})
}









#
#

test_positives <- function (positives, case, info, state, invoking_call) {
	#

	for (prop_group in positives) {

		prop_predicate <- prop_group[[1]]

		# -- we don't care if the precondition always works;
		# -- accept errors and non-boolean values
		is_match <- tryCatch(
			do.call(prop_predicate, case),
			warning =
				function (warn) False,
			error =
				function (err) False
		)

		# -- the precondition doesn't match,
		# -- so don't check the properties.

		state $ positive_case_examined <-
			state $ positive_case_examined + 1

		if (!isTRUE(is_match)) {
			next
		}

		state $ positive_tests_run <-
			state $ positive_tests_run + 1

		for (property in tail(prop_group, -1)) {

			has_property <- do.call(property, case)

			# -- to aid debugging.
			if (length(has_property) != 1) {

				message <-
					info %+% '\n' %+%
					'the property ' %+% ddparse(body(property)) %+%
					' returned a non length-one result\n' %+%
					'For the test case ' %+% ddparse(case)

				throw_arrow_error(invoking_call, message)
			}

			# -- the result of the property must always be true or false.
			if (!isTRUE(has_property) && !identical(has_property, False)) {

				message <-
					info %+% '\n' %+%
					'the property ' %+% ddparse(body(property)) %+%
					' returned a non-logical result\n' %+%
					'For the test case ' %+% ddparse(case)

				throw_arrow_error(invoking_call, message)
			}

			# -- the result must be true to pass.
			if (!isTRUE(has_property)) {

				# -- make sure the earliest failure is noted.
				state $ positive_failed_after <-
					min(state $ positive_failed_after, state $ positive_tests_run)

				# -- store the failed case.
				state $ positive_fails_for <-
					c(state $ positive_fails_for, list(case))
			}

		}
	}

	state
}









#
#

test_negatives <- function (negatives, case, info, state, invoking_call) {

	for (failprop in negatives) {
		given <- failprop[[1]]

		is_match <- tryCatch(
			do.call(given, case),
			warning =
				function (warn) False,
			error =
				function (err) False
		)

		state $ negative_case_examined <-
			state $ negative_case_examined + 1

		if (!isTRUE(is_match)) {
			next
		}

		# -- test each property that should fail for this case.
		for (fail in tail(failprop, -1)) {

			case_fails <- tryCatch({
				do.call(fail, case)
				False
				},
				warning = function (warn) True,
				error   = function (err) True
			)

			if (!isTRUE(case_fails)) {

				state $ no_fail_after <-
					min(no_fail_after, state$tests_run)

				state $ no_fail_for <- c(state $ no_fail_for, list(case))

			}
		}
	}
}









# Generate a random test case for each parametre we decided to
# test over.

yield_case <- function (params) {
	lapply(seq_along(params), from_stream)
}









# Positive controls failed.
#

throw_positive_errors <- function (info, state, invoking_call) {

	if (state $ positive_tests_run == 0) {

		run      <- state $ positive_tests_run
		examined <- state $ positive_case_examined

		message <- info %+% "\nFailed; all " %+% examined %+% " test cases" %+%
			" were rejected."

		throw_arrow_error(invoking_call, message)
	}

	if (length(state $ positive_fails_for) > 0) {

		after     <- state $ positive_failed_after
		fails_for <- state $ positive_fails_for

		# -- remove keys to simplify output.
		cases <- vapply(lapply(fails_for, unname), ddparse, character(1))

		cases <-
			paste0(cases[ seq_along( min(10, length(cases)) ) ],
			collapse = "'\n")

		message <- info %+% "\nFailed after the " %+%
			ith_suffix(after) %+% " case!\n\n" %+% cases %+% "\n"

		throw_arrow_error(invoking_call, message)

	}
}









# Negative controls failed.
#

throw_negative_errors <- function (info, state, invoking_call) {

	if (state $ negative_tests_run == 0) {

		run      <- state $ negative_tests_run
		examined <- state $ negative_case_examined

		message <- info %+% "\nFailed; all " %+% examined %+% " test cases" %+%
			" were rejected."

		throw_arrow_error(invoking_call, message)
	}

	if (length(state $ negative_fails_for) > 0) {

		after <- state $ no_fail_after
		no_fail_for <- state $ no_fail_for

		cases <- vapply(lapply(no_fail_for, unname), ddparse, character(1))

		cases <-
			paste0(cases[ seq_along( min(10, length(cases)) ) ],
			collapse = "'\n")

		message <- info %+% "\nFailed after the " %+%
			ith_suffix(after) %+% " case!\n\n" %+% cases %+% "\n"

		throw_arrow_error(invoking_call, message)

	}
}







# Everything worked. Report the fact.

state_sucess <- function (state, info) {
	# -- report that the tests all passed.

	msg <- info %+% " passed! (" %+% state $ positive_tests_run %+% ")"
	message(msg)

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

	# -- add parametres to each failure and property.
	positives   <- parameterise(positives, params, parent_frame)
	negatives   <- parameterise(negatives, params, parent_frame)

	# -- the state that is modified after running several tests.

	state <- list(
		positive_case_examined = 0,
		positive_tests_run     = 0,
		positive_fails_for     = list(),
		positive_failed_after  = Inf,

		negative_case_examined = 0,
		negative_tests_run     = 0,
		negative_fails_for     = list(),
		negative_failed_after  = Inf,

		time_left              = xStopwatch(time)
	)


	# -- test random test cases for a preset amount of time.
	while (state $ time_left()) {

		# -- generate a random test case.
		case <- yield_case(params)

		state <- test_positives(positives, case, info, state, invoking_call)
		#state <- test_negatives(negatives, case, info, state, invoking_call)
	}

	throw_positive_errors(info, state, invoking_call)
	throw_negative_errors(info, state, invoking_call)

	state_sucess(state, info)
}










# -------------------------------- Grammar -------------------------------- #

# describe(str):                       add a description (singleton field).
# over(...symbols):              give the parametres to be bound (singleton field).

# check(expr):                   add a single predicate to test.
# checkWhen(expr, expr):         add a single predicate to test of a subset of the domain.

# fails(expr, [str]):            add a single function to test for expected failure.
# failsWhen(expr, expr, [str]):  add a single function to test for expected failure, over a limited domain.

# chain with '+': it('this is a test') + over(x, y) + go()
#

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

	params <- vapply(symbols, toString, character(1))

	out <- list(params = params)
	class(out) <- c('xforall', 'xover')
	out
}






# -- test positives (+ controls)
#
#

when <- function (expr1, ...) {

	invoking_call <- sys.call()

	exprs <- as.list(match.call(expand.dots = False)[-1])

	if (missing(..1)) {
		message <-
			'when must specify expectations.'

		throw_arrow_error(invoking_call, message)
	}

	out <- list(
		positives =
			c(list( exprs[[1]] ), exprs$...)
	)
	class(out) <- c('xforall', 'xwhen')
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
			'when must specify expectations.'

		throw_arrow_error(invoking_call, message)
	}

	out <- list(
		negatives =
			c(list( exprs[[1]] ), exprs$...)
	)
	class(out) <- c('xforall', 'xfailswhen')
	out

}




# Run specifies that the test object should now be
# executes. Also specifies how long to run the test for.

run <- function (time = 1) {
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
		'xdescribe'  = override('info'),
		'xover'      = override('params'),
		'xwhen'      = join('positives'),
		'xfailswhen' = join('negatives'),
		'xrun'       = function () {
			acc $ time <- new $ time

			execute_test(acc)
		}
	)

	new_classes <- class(new)

	for (classname in names(responses)) {
		if (any(new_classes == classname)) {
			return ( responses[[classname]]() )
		}
	}
}
