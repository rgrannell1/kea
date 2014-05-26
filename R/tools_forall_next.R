
from_stream <- function () {
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

	# -- with that out of the way, yield a value.

	implemented <- ls(envir = this)

	sampler <- this[[ rsample(implemented, size = 1) ]]
	sampler()
}











execute_test <- function (test) {

	invoking_call <- sys.call()
	parent_frame <- parent.frame()

	# -- attach the test object to the local environment.
	info       <- test $ info
	params     <- test $ params
	properties <- test $ properties
	time       <- test $ time

	# -- throw an error if any test fields are missing.
	for (key in c('info', 'params', 'properties')) {
		if (is.null( test[[key]] )) {
			message <-
				'the property ' %+% key %+% ' is missing from the test object.'

			throw_arrow_error(invoking_call, message)
		}
	}

	# -- validate the test fields
	if (!is.character(info) || length(info) != 1) {
		message <-
			'the property info must be a length-one character vector.'

		throw_arrow_error(invoking_call, message)
	}

	if (!is.numeric(time) || length(time) < 0) {
		message <-
			'time must be a positive number'

		throw_arrow_error(invoking_call, message)
	}

	info <- dQuote(info)

	# -- parameterise all the expressions
	properties <- lapply(properties, function (prop) {
		lapply(prop, function (expr) {

			shell <- function () {}

			# -- add the parameters given by over, and use the parent env.
			body(shell) <- expr
			environment(shell) <- parent_frame
			formals(shell) <- make_formals(params)

			shell
		})
	})

	# -- the state that is modified after running several tests.

	state <- list(
		tests_run = 0,
		fails_for = list(),
		failed_after = Inf,
		time_left = xStopwatch(time)
	)

	# -- test random test cases for a preset amount of time.
	while (state$time_left()) {

		# -- generate a random test case.
		case <- lapply(seq_along(params), function (x) {
			from_stream()
		})

		# -- check every property group with the case
		for (prop in properties) {

			given <- prop[[1]]

			# -- we don't care if the precondition always works;
			# -- accept errors and non-boolean values
			is_match <- tryCatch(
				do.call(given, case),
				warning =
					function (warn) False,
				error =
					function (err) False
			)

			# -- the precondition doesn't match, so don't check the expectations.
			if (!isTRUE(is_match)) {
				next
			}

			# -- the tests will be run once each.
			state$tests_run <- state$tests_run + 1

			# -- if the precondition matches iterate over each expectation.
			for (expect in tail(prop, -1)) {

				has_property <- do.call(expect, case)

				# -- to aid debugging.
				if (length(has_property) != 1) {
					message <- ''
				}

				# -- this must always be true or false.
				if (!isTRUE(has_property) && !identical(has_property, False)) {

					message <-
						info %+% '\n' %+%
						'the property ' %+%
						ddparse(body(expect)) %+%
						' returned a non-logical result\n' %+%
						'For the test case ' %+%
						ddparse(case)

					throw_arrow_error(invoking_call, message)
				}

				# -- the result must be true to pass.
				if (!isTRUE(has_property)) {

					# -- make sure the earliest failure is noted.
					state$failed_after <-
						min(state$failed_after, state$tests_run)

					# -- store the failed case.
					state$fails_for <- c(state$fails_for, list(case))

				}
			}

		}
	}

	# we have a problem; the test failed.
	if (length(state$fails_for) > 0) {

		after <- state  $ failed_after
		fails_for <- state $ fails_for

		# -- remove keys to simplify output.
		cases <- vapply(lapply(fails_for, unname), ddparse, character(1))

		cases <-
			paste0(cases[ seq_along( min(10, length(cases)) ) ],
			collapse = "'\n")

		message <- info %+% "\nFailed after the " %+%
			ith_suffix(after) %+% " case!\n\n" %+% cases %+% "\n"

		throw_arrow_error(invoking_call, message)
	}

	message(info, " passed!", " (", state$tests_run, ")")
}










# Grammar

# it(str):                       add a description (singleton field).
# over(...symbols):              give the parametres to be bound (singleton field).

# check(expr):                   add a single predicate to test.
# checkWhen(expr, expr):         add a single predicate to test of a subset of the domain.

# fails(expr, [str]):            add a single function to test for expected failure.
# failsWhen(expr, expr, [str]):  add a single function to test for expected failure, over a limited domain.

# chain with '|': it('this is a test') | over(x, y) | go()
#

# -- the description

it <- function (info) {
	out <- list(info = info)
	class(out) <- c('xforall', 'xit')
	out
}

# -- the domain over which to bind

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

# -- test properties (+ controls)

when <- function (expr1, ...) {

	exprs <- as.list(match.call(expand.dots = False)[-1])

	out <- list(
		properties =
			c(list( exprs[[1]] ), exprs$...)
	)
	class(out) <- c('xforall', 'xwhen')
	out

}

# -- test failures (- controls)

fails <- function (expr) {

}

failsWhen <- function () {

}

run <- function (time = 2) {
	out <- list(time = time)
	class(out) <- c('xforall', 'xrun')
	out
}










'|.xforall' <- function (acc, new) {
	# a monoidal operation that joins any
	# member of 'xforall' into a compound object,
	# unless run is joined.
	# run signals execution.


	responses <- list(
		'xit' =
			function () {
				acc $ info = new $ info
				acc
			},
		'xover' =
			function () {
				acc $ params = new $ params
				acc
			},
		'xwhen' =
			function () {
				if (length(acc $ properties) > 0) {
					acc $ properties <- c(acc $ properties, list(new $ properties))
				} else {
					acc $ properties <- list(new $ properties)
				}
				acc
			},
		'xrun' =
			function () {
				# -- execute the test.

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







# ----------------------- The Actual Implementation ----------------------- #
#
# Given a list representing a test program

if (False) {

over(x) |
    it('is always divisible by itself') |
    when(is.numeric(x) && length(x) > 0 && is.finite(x) && x != 0, x/x == 1) |
    run()

}
