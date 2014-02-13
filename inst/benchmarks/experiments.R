
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
