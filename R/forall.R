
# -------------------------------- G -------------------------------- #
#
# this structure contains random-testcase generating thunks.
# these are primarily going to be used for forall-based testing.
#

G <- local({

	tools <- local({

		this <- list()
		
		this$one_of <- 
			function (coll) {
			# select a single value from a collection.

			ith <- sample(seq_along(coll), size = 1)
			coll[[ith]]
		}

		this$combine <-
			function (...) {
				# combine several thunks into one thunk, that
				# yields one of the underlying thunks. thunk thunk thunk.

				fns <- list(...)

				function () {

					assert(
						all( sapply(fns, is.function) ), pcall,
						lament$non_function_cases(info))

					this$one_of(fns)()
				}
			}

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

	this <- list()

	# ---------------- non-parameterised functions ---------------- #

	# -------- letters -------- #

	this$letter <-
		function () {
			tools$one_of(letters)
		}

	# -------- logical values -------- #

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

	# -------- logical functions -------- #
	
	this$truth <-
		function () {
			function () True
		}
	this$falsity <-
		function () {
			function () False
		}
	# beats 'nonapplicability'.
	this$mu <-
		function () {
			function () Na
		}

	this$boolean_functions <-
		tools$combine(
			this$truth, this$falsity)

	this$logical_functions <-
		tools$combine(
			this$boolean_functions, this$mu)

	# -------- empty data structures -------- #

	this$recursive_zero <-
		function () {
			tools$one_of( list(NULL, list()) )
		}
	this$typed_vector_zero <-
		function () {
			tools$one_of(list(
				integer(), character(), 
				raw(), logical(), numeric()) )
		}
	this$collection_zero <-
		tools$combine(
			this$typed_vector_zero,
			this$recursive_zero)





	# ---------------- parameterised functions ---------------- #
	#
	# these function generators need additional parameters to  
	# construct their return function. This usually includes
	# the standard deviation of the length or magnitude of
	# their ultimate return value. 
	#

	# -------- number functions -------- #

	this$integer <-
		function (sd = 20) {
			function () {
				round(rnorm(1, 0, sd), 0)
			}
		}
	this$nonnegative <-
		function (sd = 20) {
			function () {
				abs(round(rnorm(1, 0, sd), 0))				
			}
		}
	this$positive <-
		function (sd = 20) {
			function () {
				abs(round(rnorm(1, 0, sd), 0)) + 1				
			}
		}

	# -------- character functions -------- #

	this$word <-
		function (sd = 20) {
			function () {
				size <- abs(round(rnorm(1, 0, sd), 0)) + 1
				paste0(
					sample(letters, size = size, replace = True),
					collapse = "")
			}
		}

	# -------- collection functions -------- #
	#
	# this functions take length-one and length-zero generators,
	# and combine them to create vectors of arbitrary length and/or/depth.
	#

	this$words <-
		function (sd = 20) {
			tools$vector_of(this$word, sd)			
		}

	this$integers <-
		function (sd = 20) {
			tools$vector_of(this$integer, sd)			
		}

	this$integer_seq <-
		function (sd = 20) {
			function () {
				size <- abs(round(rnorm(1, 0, sd), 0)) + 1
				seq_len(size)
			}
		}






	this
})











# -------------------------------- forall -------------------------------- #
#
# forall tests if an expression holds true over a range of random test-cases.
#

forall <- function (info = "", cases, expect, given, max_time = 0.1) {

	pcall <- sys.call()

	assert(
		all( sapply(cases, is.function) ), pcall,
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
				result %in% c(True, False), pcall,
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
		pcall,
		lament$failed_cases(
			info,
			state$failed_after, 
			state$failed))

	message(info, " passed!", " (", state$tests_run, ")")
}
