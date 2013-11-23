





atoms <- local({
	# functions that generate a single value.

	this <- object()

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
			one_of(
				function () True,
				function () False,
				function () Na
			)
		}

	this$boolean_function <-
		function () {
			one_of(
				function () True,
				function () False
			)
		}

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
				size <- abs(round(rnorm(1, 0, sd), 0)) + 1

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

	this <- object()

	this$vector_of <-
		function (fn, sd = 20) {
			function () {

				len <- abs(round(rnorm(1, 0, sd), 0)) + 1

				coll <- vector()
				while (length(coll) < len) {
					val <- fn()()
					coll <- c(coll, val)
				}
				coll
			}
		}

	this$list_of <-
		function (fn, sd = 20) {
			function () {

				len <- abs(round(rnorm(1, 0, sd), 0)) + 1

				coll <- list()
				while (length(coll) < len) {
					val <- list( fn()[[ 1 ]] )
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

	this <- object()

	# --------------------- Empty Collections --------------------- #

	# empty recursive structures.

	this$recursive_zero <-
		function () {
			one_of( list(Null, list()) )
		}
	# empty vectors.

	this$vector_zero <-
		function () {
			one_of(list(
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

				one_of(
					function () character(),
					as_coll$vector_of(atoms$word, sd))(  )
			}
		}

	this$integers <-
		function (sd = 20) {
			function () {

				one_of(
					function () integer(),
					as_coll$vector_of(atoms$integer, sd))(  )
			}
		}

	this$logicals <-
		function (sd = 20) {
			function () {

				one_of(
					function () logical(),
					as_coll$vector_of(atoms$logical, sd))(  )
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
		function (sd) {

			as.list(one_of(list(
				this$words(sd),
				this$integers(sd),
				this$logicals(sd)
			))(  ))
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

	this <- object()

	this$empty <-
		list(coll = G$collection_zero)

	this$mod2_over_ints <-
		list(
			fn =
				function () {
					function (num) num %% 2 == 0
				},
			coll =
				compounds$integers()
		)
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
			fn = atoms$mu,
			coll = compounds$collection)

	this$inc_over_ints <-
		list(
			fn = function () {
				function (x) x + 1
			},
			coll = compounds$integers())




	this
})
















G <- local({

	this$standard <- local({

		this <- list()







		this$logical_with_collection_zero <-
			list(
				fn = G$logical_functions,
				coll = G$collection_zero
			)

		this$coll <-
			function () {
				list(coll = G$collection())
			}

		this$two_colls <-
			function () {
				list(coll1 = G$collection(), coll2 = G$collection())
			}

		this$two_colls_left_empty <-
			function () {
				list(coll1 = G$collection_zero, coll2 = G$collection())
			}

		this$two_colls_right_empty <-
			function () {
				list(coll1 = G$collection(), coll2 = G$collection_zero)
			}
		this
	})


	this
})









# -------------------------------- forall -------------------------------- #
#
# forall tests if an expression holds true over a range of random test-cases.
#

forall <- function (info = "", cases, expect, given, max_time = 0.1) {

	parent_call <- sys.call()

	assert(
		all( sapply(cases, is.function) ), parent_call,
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
		time_left = xTimer(max_time),
		failed = list())

	while (state$time_left()) {

		case <- lapply(cases, function (fn) fn())

		if (do.call(given, case)) {

			state$tests_run <- state$tests_run + 1
			result <- do.call(expect, case)

			assert(
				length(result) == 1, parent_call,
				lament$non_singular_expectation(info, length(result)) )

			assert(
				result %in% c(True, False), parent_call,
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
		parent_call,
		lament$failed_cases(
			info,
			state$failed_after,
			state$failed))

	message(info, " passed!", " (", state$tests_run, ")")
}
