
require(arrow)
require(microbenchmark)

# ------------------ Calibration & Controls ------------------


# ------------------ Experiment One  ------------------
# What is the fastest method of making indices?
#
# seq performs badly for small n, asymptotically levels out.
# seq_along seems to be the slightly faster method.
# for small n, seq_along is about one second every million iterations faster.
# this isn't much, but it's perhaps worth keeping in mind.

local({

	low <-  100
	mid <-  1000
	high <- 1000000

	one_to_low <-  1:low
	one_to_mid <-  1:mid
	one_to_high <- 1:high

	microbenchmark(
		seq_along(one_to_low),
		seq_along(one_to_mid),
		seq_along(one_to_high),

		seq_len(length(one_to_low)),
		seq_len(length(one_to_mid)),
		seq_len(length(one_to_high)),

		1:(length(one_to_low)),
		1:(length(one_to_mid)),
		1:(length(one_to_high)),

		seq(1, length(one_to_low)),
		seq(1, length(one_to_mid)),
		seq(1, length(one_to_high)),

		unit = 'us', times = 10000, control = list(warmup = 100)
	)
})

# ------------------ Experiment Two  ------------------
# Do return statements hurt performance?
#
# no, if they do the time lost is 1 second per 20 million iterations.
# the use of return didn't seem to affect speed at all.

local({

	f1 <- function () {

		if (FALSE) return()
	}

	f2 <- function () {
		if (FALSE) {

		}
	}

	g1 <- function () {
		if (TRUE) return (letters)
	}

	g2 <- function () {
		if (TRUE) {
			letters
		}
	}

	microbenchmark(
		f1(),
		f2(),

		g1(),
		g2(),
		unit = 'ns', times = 1000000, control = list(warmup = 100)
	)
})

# ------------------ Experiment Two  ------------------
# Are sys call and match call equally fast? How fast is sys call?
#
# match.call was twice as slow for the test case.
# the use of sys.call costs about a second every 500,000 iterations.

local({

	f1 <- function (...) {
		invoking_call <- sys.call()
	}
	g1 <- function (...) {
		invoking_call <- match.call()
	}

	microbenchmark(
		f1(letters),
		g1(letters),

		unit = 'ns', times = 10000, control = list(warmup = 100)
	)
})

# ------------------ Experiment Three  ------------------
# How long does it take to test for missing arguments, versus missing?
#
# testing for missing args is really slow.
# it costs about one second every 27,000 iterations.
#
# stopifnot is also slow, 70,000 Hz.
#
# force is fast - 500,000 Hz.
#
# FIXED: new solution is shorter, and is 200,000 Hz.

local({

	#f1 <- function (fn) {
	#	assert(
	#		!missing(fn), invoking_call,
	#		exclaim$parametre_missing(fn))
	#}

	current <- function (fn) {
		insist $ must_not_be_missing(fn)
	}

	base_assert <- function (fn) {
		stopifnot(!missing(fn))
	}

	not_missing <- function (fn) {
		!missing(fn)
	}

	forced <- function (fn) {
		force(fn)
	}

	microbenchmark(
		#f1(letters),
		current(letters),
		base_assert(letters),
		not_missing(letters),
		forced(letters),

		unit = 'ns', times = 10000, control = list(warmup = 100)
	)
})

# ------------------ Experiment Four  ------------------
#
# How fast is assert, in comparison to stopifnot.

# assert is 25,000Hz: quite slow
# stopifnot is about 70,000Hz

# removed erroneous as.list match call - now 130,000Hz

local({

	invoking_call <- sys.call()

	current <- function () {
		assert(TRUE, invoking_call, "message")
	}

	base_assert <- function () {
		stopifnot(TRUE)
	}

	microbenchmark(
		current(),
		base_assert(),

		unit = 'ns', times = 100000, control = list(warmup = 100)
	)
})

# ------------ Experiment Five ------------------

# How fast is as_typed_vector?
#
# as_typed_vector is really realy slow.
# it is 16,000 Hz compared to as.doubles 127,000 Hz for the same data.
#
# Increased to 20,000Hz by removing match call, and refactoring
# module to function.
#
# Refactored assertion checking into the insist module,
# increasing speed to 50kHz.
#
# corrected unit test 60kHz for atomics.
# generics are very slow: 550Hz.
# now 50Hz!

local({

	one_to_1000 <- 1:1000
	one_to_1000_list <- as.list(1:1000)

	current_vector <- function () {
		as_typed_vector(one_to_1000, 'integer')
	}
	as_double <- function () {
		as.double(one_to_1000)
	}
	current_list <- function () {
		as_typed_vector(one_to_1000_list, 'integer')
	}
	vapply_control <- function () {
		vapply(one_to_1000_list, function (x) T, logical(1))
	}

	unlist_doubles <- function () {
		unlist(one_to_1000_list)
	}
	as_vector_doubles <- function () {
		unlist(one_to_1000_list)
	}

	microbenchmark(
		current_vector(),
		as_double(),
		current_list(),
		vapply_control(),
		unlist_doubles(),
		as_vector_doubles(),

		unit = 'ns', times = 10000, control = list(warmup = 100)
	)
})

# ------------ Experiment Six ------------------

# What is the best way to test for membership?
#
# all very similar for large n, but for the smallest n
# with the worst case anyequal is faster.

local({

	is_elem <- function () {
		is.element('a', 1:1000)
	}

	inn <- function () {
		'a' %in% 1:1000
	}

	anyequal <- function () {
		any('a' == 1:1000)
	}

	microbenchmark(
		is_elem(),
		inn(),
		anyequal(),

		unit = 'ns', times = 10000, control = list(warmup = 100)
	)
})

# ------------ Experiment Seven ------------------

# Is as atom faster than typed vector for single values?

local({

	atom <- function () {
		as_atom(list(1), 'numeric')
	}
	typed_vector <- function () {
		as_typed_vector(list(1), 'numeric')
	}

	microbenchmark(
		atom(),
		typed_vector(),

		unit = 'ns', times = 10000, control = list(warmup = 100)
	)
})

# ------------ Experiment Eight ------------------

# xAsChars seems to be rate limiting at times.
# Is it much slower than splitstring?
#
# It isn't: both functions aren't terribly fast.
#
# xChars 9,300 hz
# splitstring 15,800hz
#
# The difference is likely down to type conversion.

local({

	string <- paste0(letters, collapse = '', size = 100, replace = True)

	f <- function (s) {
		s <- strsplit(string, '')[[1]]
		s[nchar(s) > 0]
	}

	microbenchmark(
		xToChars(string),
		f(string),

		unit = 'ns', times = 10000, control = list(warmup = 100)
	)

})


## Experiment Nine
#
# Is uncalled code non-performant?

local({

	f <- function () {
		if (FALSE) {
			x <- quote(mean(max, mode, mean, model))
			x <- quote(mean(max, mode, mean, model))
			x <- quote(mean(max, mode, mean, model))
			x <- quote(mean(max, mode, mean, model))
			x <- quote(mean(max, mode, mean, model))
			x <- quote(mean(max, mode, mean, model))
			x <- quote(mean(max, mode, mean, model))
			x <- quote(mean(max, mode, mean, model))
			x <- quote(mean(max, mode, mean, model))
			x <- quote(mean(max, mode, mean, model))
			x <- quote(mean(max, mode, mean, model))
			x <- quote(mean(max, mode, mean, model))
			x <- quote(mean(max, mode, mean, model))
			x <- quote(mean(max, mode, mean, model))
			x <- quote(mean(max, mode, mean, model))
			x <- quote(mean(max, mode, mean, model))
			x <- quote(mean(max, mode, mean, model))
			x <- quote(mean(max, mode, mean, model))
			x <- quote(mean(max, mode, mean, model))
			x <- quote(mean(max, mode, mean, model))
			x <- quote(mean(max, mode, mean, model))
			x <- quote(mean(max, mode, mean, model))
			x <- quote(mean(max, mode, mean, model))
			x <- quote(mean(max, mode, mean, model))
		}
	}

	g <- function () {
		if (FALSE) {

		}
	}

	microbenchmark(
		f(), g(),

		unit = 'ns', times = 10000, control = list(warmup = 100)
	)

})

# runif is slow, and time functions are slower. Fastes
# method is updating counters, though its still slow

local({

	rep <- function () {
		n <- 0

		list(
			iter = function () {
				n <<- n + 1

				if (n > 10) {
					# print
				}
		})
	}
	reporter <- rep()

	rep2 <- function (iter) {
		if (runif(1) > 0.99995) {
			# print
		}
	}

	loop1 <- function () {
		for (ith in 1:1000) reporter$iter()
	}

	loop2 <- function () {
		for (ith in 1:1000) rep2(ith)
	}

	microbenchmark(
		loop1(),
		loop2(),

		unit = 'ns', times = 10000, control = list(warmup = 100)
	)
})
