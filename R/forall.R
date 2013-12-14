
# -------------------------------- forall -------------------------------- #
#
# To Developers,
#
# The forall() set of tools allows Quickcheck-style unit testing to
# verify that arrow's functions are working correctly. The system is organised
# as follows:
#
# forall:
#
#    forall( ) is a function with which unit tests are written. A typical call
#    might look like...
#
#    forall(
#        'check addition commutativity',
#        list(a = function ( ) runif(1), b = function ( ) runif(1),
#        a + b == b + a)
#
#    A description should be given of the test. The argument 'cases'
#    is the part that supplies random test cases. It is a named list of functions,
#    each of which returns a random test-case.
#
#    In this case, the variables a & b are bound with a random number.
#
#    The final argument used here is 'expect'; an expression that
#    must return true for the test to pass. Here, the expression
#    a + b == b + a must be true for the test to pass. Again, this expression
#    must evaluate to a boolean value, and true in order to pass.
#
#    forall will re-evaluate the expression with a & b re-bound to different
#    numbers; this means that the expression will be checked to be true for a
#    significant sample of random numbers.
#
#    there is a way to add counterindications to running the unit test.
#    for example,
#
#    forall(
#        'check addition commutativity',
#        list(a = function ( ) runif(1), b = function ( ) runif(1),
#        a * b > 0,
#        given =
#            a != 0 && b != 0)
#
#    the unit test above is run given that a and b aren't zero. Expression
#    'a * b > 0' won't even be run; a new random test case will instead be generated.
#
#
#    forall will continue to execute random tests for 0.1 seconds, unless the
#    time is extended. This is to keep the R CMD check runtime's prompt.
#
#
#
#
#
# atoms:
#
#    Writing test cases by hand is a pain, one which forall intents to circumevent.
#    Instead, test case generators are used. These are functions that return
#    a particular type of test case; for example random numbers, lists of varying lengths
#    of random strings.
#
#    There are two levels of test case generators: atoms (length-one values) and
#    compounds (collections of said values).
#
#    Atoms is an environment (an R object) of functions that return random test cases.
#
#
#
#
#
# as_coll:
#
#    as_coll is an environment that contains functions that take an atom generator, and
#    returns a compound generator function. For example, atom$integer creates a random integer.
#    compound$integers returns a variable-length vector of such integers.
#
#    This is solely included to shorten the (annoying) task of writing compound
#    test case generators.
#
#
#
#
#
# compounds:
#
#    Generators for lists and vectors of select values.
#
#
#
#
#
# test_cases:
#
#    Ultimately, the function forall( ) takes generator functions and binds their results to
#    variable name. In order to reduce repetition, test_cases contains an environment
#    of 'pre-approved' test_cases lists. For example, a lot of arrow functions take a
#    boolean function and a collection. It is prudent to test these functions with a logical function
#    and an empty collection of any type, so a pre-approved test case is contained in this
#    object for just that purpose.
#

atoms <- local({
	# functions that generate a single value.

	this <- Object()

	# --------------------- Boolean --------------------- #

	this$true <-
		function () {
			True
		}
	this$false <-
		function () {
			False
		}
	this$na <-
		function () {
			Na
		}

	# multiple logical values.

	this$boolean <-
		function () {
			one_of(c(True, False))
		}
	this$logical <-
		function () {
			one_of(c(True, False, Na))
		}

	# single logical functions.

	this$truth <-
		function () {
			function (...) True
		}
	this$falsity <-
		function () {
			function (...) False
		}
	this$moot <-
		function () {
			function (...) Na
		}

	# multiple logical functions.

	this$logical_function <-
		function () {
			one_of(list(
				function () True,
				function () False,
				function () Na
			))
		}

	this$boolean_function <-
		function () {
			one_of(list(
				function () True,
				function () False
			))
		}

	# --------------------- Linear Function --------------------- #

	this$linear_function <-
		function (sd = 20) {
			function (num) {
				abs(round(rnorm(1, 0, sd), 0)) + 1 * num
			}
		}

	# --------------------- Base Functions --------------------- #

	this$base_primitive <- local({

		fns <- Filter(
			function (fn) {
				is.function(fn) && is.primitive(fn)
			},
			lapply(ls('package:base'), get)
		)

		function () {
			one_of(fns)
		}
	})

	this$base_function <- local({

		fns <- Filter(
			function (fn) {
				is.function(fn) && !is.primitive(fn)
			},
			lapply(ls('package:base'), get)
		)

		function () {
			one_of(fns)
		}
	})

	# --------------------- Infinity --------------------- #

	this$positive_infinity <-
		function () {
			+Inf
		}

	this$negative_infinity <-
		function () {
			-Inf
		}

	# multiple logical values.

	this$infinity <-
		function () {
			one_of(c(-Inf, +Inf))
		}

	# --------------------- Number --------------------- #

	# integers.

	this$integer <-
		function (sd = 20) {
			function () {
				round(rnorm(1, 0, sd), 0)
			}
		}
	this$nonnegative_integer <-
		function (sd = 20) {
			function () {
				abs(round(rnorm(1, 0, sd), 0))
			}
		}
	this$positive_integer <-
		function (sd = 20) {
			function () {
				abs(round(rnorm(1, 0, sd), 0)) + 1
			}
		}

	# --------------------- Character --------------------- #

	# single letters.

	this$letter <-
		function () {
			one_of(letters)
		}

	# single words.

	this$word <-
		function (sd = 20) {
			function () {
				size <- abs(round(rnorm(1, 0, sd), 0))

				paste0(
					sample(letters, size = size, replace = True),
					collapse = "")
			}
		}

	this
})










as_coll <- local({
	# combine atomic generators into generators
	# for a collections of atoms

	this <- Object()

	this$vector_of <-
		function (fn, sd = 20) {
			function () {
				# convert a atom function to a vector of such atoms.

				len <- abs(round(rnorm(1, 0, sd), 0)) + 1

				coll <- vector()
				while (length(coll) < len) {

					val <- fn()
					coll <- c(coll, val)
				}
				coll
			}
		}

	this$list_of <-
		function (fn, sd = 20) {
			function () {
				# convert a atom function to a list of such atoms.

				len <- abs(round(rnorm(1, 0, sd), 0)) + 1

				coll <- list()
				while (length(coll) < len) {

					val <- list( fn() )
					coll <- c(coll, val)
				}
				coll
			}
		}

	this$collection_of <-
		function (fn, sd = 20) {
			function () {
				# convert a atom function to a collection of such atoms.

				to_vector <- sample(c(True, False), size = 1)

				coll_fn <- if (to_vector) {
					as.vector
				} else {
					list
				}

				coll <- if (to_vector) {
					vector()
				} else {
					list()
				}

				len <- abs(round(rnorm(1, 0, sd), 0)) + 1

				while (length(coll) < len) {

					# the function should return
					# an instance immediately.

					val <- coll_fn( fn() )
					coll <- c(coll, val)
				}
				coll
			}
		}

	this

})









compounds <- local({
	# functions that return vectors and recursive data types.
	# these collections themselves are generated using
	# the atomic generator functions.

	this <- Object()

	# --------------------- Empty Collections --------------------- #

	# empty recursive structures.

	this$recursive_zero <-
		function () {
			one_of( list(Null, list()) )
		}
	# empty vectors.

	this$vector_zero <-
		function () {
			one_of( list(
				integer(), character(),
				raw(), logical(), numeric()) )
		}

	# empty vectors or recursives.

	this$collection_zero <-
		function () {
			one_of(list(
				Null, list(), integer(),
				character(), logical(),
				raw(), numeric()
			))
		}

	# --------------------- Typed Vectors --------------------- #

	this$words <-
		function (sd = 20) {
			function () {

				one_of( list(
					function () character(),
					as_coll$vector_of(atoms$word(), sd)) )(  )
			}
		}

	this$integers <-
		function (sd = 20) {
			function () {

				one_of( list(
					function () integer(),
					as_coll$vector_of(atoms$integer(), sd)) )(  )
			}
		}

	this$logicals <-
		function (sd = 20) {
			function () {

				one_of( list(
					function () logical(),
					as_coll$vector_of(atoms$logical, sd)) )(  )
			}
		}

	this$vector <-
		function (sd) {

			one_of(list(
				this$words(sd),
				this$integers(sd),
				this$logicals(sd)
			))(  )
		}

	this$collection <-
		function (sd = 20) {

			one_of(list(
				this$words(sd),
				this$integers(sd),
				this$logicals(sd)
			))(  )
		}

	this$collection_of_length_zero <-
		function (sd = 20) {
			# generate a collection of length-zero values.

			as_coll$list_of(this$collection_zero, sd)


		}

	# --------------------- Special Collections --------------------- #

	this$integer_seq <-
		function (sd = 20) {
			function () {

				size <- abs(round(rnorm(1, 0, sd), 0)) + 1
				seq_len(size)
			}
		}

	this

})







test_cases <- local({
	# pre-approved test cases.

	this <- Object()

	this$mod2_over_ints <-
		list(
			fn =
				function () {
					function (num) num %% 2 == 0
				},
			coll =
				compounds$integers()
		)

	this$sum_over_integers <-
		list(
			fn = function () match.fun("+"),
			coll = compounds$integers())

	this$id_over_collection_zero <-
		list(
			fn = function () {
				function (...) list(...)
			},
			coll = compounds$collection_zero)

	this$succ_over_integers <-
		list(
			fn = function () {
				function (x) x + 1
			},
			coll = compounds$integers())

	this$left_over_collection <-
		list(
			fn = function () {
				function (a, b) a
			},
			coll = compounds$collection)

	this$right_over_collection <-
		list(
			fn = function () {
				function (a, b) b
			},
			coll = compounds$collection)

	# --------------------- Num + Coll ----------------------------- #

	this$nonnegative_with_collection_zero <-
		list(
			num = atoms$nonnegative_integer(),
			coll = compounds$collection_zero)

	this$positive_with_collection_zero <-
		list(
			num = atoms$nonnegative_integer(),
			coll = compounds$collection_zero)

	this$positive_with_recursive_zero <-
		list(
			num = atoms$nonnegative_integer(),
			coll = compounds$recursive_zero)

	this$positive_with_collection <-
		list(
			num = atoms$positive_integer(),
			coll = compounds$collection)

	this$positive_with_linear_function <-
		list(fn = atoms$linear_function, num = atoms$positive_integer())

	# --------------------- Logical-Fun + Coll --------------------- #

	this$truth_with_coll <-
		list(
			fn = atoms$truth,
			coll = compounds$collection)

	this$falsity_with_coll <-
		list(
			fn = atoms$falsity,
			coll = compounds$collection)

	this$moot_with_coll <-
		list(
			fn = atoms$moot,
			coll = compounds$collection)

	this$logical_functions_with_collection <-
		list(
			fn = atoms$logical_function,
			coll = compounds$collection
		)

	this$logical_functions_with_collection_zero <-
		list(
			fn = atoms$logical_function,
			coll = compounds$collection_zero
		)

	# --------------------- Coll-Only --------------------- #

	this$recursive_zero <-
		list(coll = compounds$recursive_zero)

	this$collection_zero <-
		list(coll = compounds$collection_zero)

	this$collection_of_length_zero <-
		list(coll = compounds$collection_of_length_zero())

	this$collection <-
		list(coll = compounds$collection)

	this$integers <-
		list(coll = compounds$integers())

	this$infinity <-
		list(coll = atoms$infinity)

	# --------------------- num-Only --------------------- #

	this$num_integer <-
		list(
			num = atoms$integer())
	this$num_positive_integer <-
		list(num = atoms$positive_integer())

	# --------------------- fn-Only --------------------- #

	this$base_primitive <-
		list(fn = atoms$base_primitive)
	this$base_function <-
		list(fn = atoms$base_function)

	# --------------------- str-Only --------------------- #

	this$str_word <-
		list(str = atoms$word())

	this$str_words <-
		list(strs = compounds$words())

	# --------------------- str-str --------------------- #

	this$str_word_and_words <-
		list(str = atoms$word(), strs = compounds$words())

	# --------------------- col + col --------------------- #

	this$two_collection_zeros =
		list(
			coll1 = compounds$collection_zero,
			coll2 = compounds$collection_zero)

	this$two_collections =
		list(
			coll1 = compounds$collection,
			coll2 = compounds$collection)

	this
})







# -------------------------------- forall -------------------------------- #
#
# forall tests if an expression holds true over a range of random test-cases.
#

forall <- function (info = "", cases, expect, given, max_time = 0.1) {

	invoking_call <- sys.call()

	assert(
		all( sapply(cases, is.function) ), invoking_call,
		lament$non_function_cases(info))

	# ----- capture the expect and given expressions as functions

	expect_expr <- match.call()$expect
	given_expr <-
		if (missing(given)) {
			True
		} else {
			match.call()$given
		}

	expect <- given <-
		function () {}

	formals(expect) <-
		as_parametres(names(cases))

	formals(given) <-
		as_parametres(names(cases))

	body(expect) <- expect_expr
	body(given) <- given_expr

	# ----- check that the expectation is true for a range of cases

	state <- list(
		tests_run = 0,
		failed_after = Inf,
		time_left = xStopWatch(max_time),
		failed = list())

	while (state$time_left()) {

		case <- lapply(cases, function (fn) fn())

		if (do.call(given, case)) {

			state$tests_run <- state$tests_run + 1
			result <- do.call(expect, case)

			assert(
				length(result) == 1, invoking_call,
				lament$non_singular_expectation(info, length(result)) )

			assert(
				result %in% c(True, False), invoking_call,
				lament$non_boolean_expectation(info, case))

			if (!result) {
				state$failed_after <-
					min(state$failed_after, state$tests_run)

				state$failed <-
					c(state$failed, list(case))
			}
		}
	}

	assert(
		length(state$failed) == 0,
		invoking_call,
		lament$failed_cases(
			info,
			state$failed_after,
			state$failed))

	message(info, " passed!", " (", state$tests_run, ")")
}
