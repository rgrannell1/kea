
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
}

# -- the domain over which to bind

over <- function (...) {

	# -- capture the symbols
	symbols <- as.list(match.call()[-1])

	# -- validate the symbols

	stopifnot( vapply(symbols, is.name, logical(1)) )

	params <- vapply(symbols, toString, character(1))

	out <- list(params = params)
}

# -- test properties (+ controls)

when <- function (expr1, expr2) {
	
	exprs <- as.list(match.call()[-1])

	out <- list(
		condition = exprs[[1]],
		expectation = exprs[[2]]
	)

}

# -- test failures (- controls)

fails <- function (expr) {

}

failsWhen <- function () {

}

run <- function (time = 0.15) {
	
}










fromStream <- function () {
	# -- yield a single valid R object.

	# -- finish this alphabet
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
				sample(extended_ascii, size = no_chars, replace = True),
				collapse = '')
		}

	this $ line <-
		function (...) {

			no_words <- abs( rnorm(1, 1, sd = 100) )

			words <- unlist(lapply(seq_len(no_words), this $ word))

			paste0(words, collapse = sample(whitespace, size = 1))
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

			sample(c(True, False, Na), size = no_bools, replace = True)
		}

	# -- symbols
	this $ symbol <-
		function () {
			as.symbol(this $ word())
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
			sample.int(2147483647, 1) * sample(c(-1, 1), size = 1)
		}

	# -- function

	# -- with that out of the way, yield a value.

	implemented <- ls(envir = this)

	sampler <- this[[ sample(implemented, size = 1) ]]
	sampler()
}
