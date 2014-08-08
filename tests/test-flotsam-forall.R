
kea ::: load_test_dependencies(environment())

message("holdsWhen")

	over(a, b) +

	describe("ordering partitions numbers") +
	holdsWhen(
		is.numeric(a)  && is.numeric(b)  &&
		length(a) == 1 && length(b) == 1 &&
		!is.na(a) && !is.na(b),
		a > b || b >= a
	) +

	describe("multiplication by 1 is identity") +
	holdsWhen(
		is.numeric(a) && length(a) == 1 &&
		is.finite(a) &&
		is.null(names(a)),
		a * 1 == a
	) +

	run(5)

message("holdsWhen failures")

	assert_throws_error <- function (expr) {

		tryDefault <- function (expr, val) {

			tryCatch(
				expr,
				warning = function (warn) val,
				error   = function (err)  val
			)

		}

		res <- tryDefault(expr, TRUE)

		if (!isTRUE(res)) {
			stop('error in test')
		}
	}


	assert_throws_error({

		over(a) +

		describe('a is not a') +
		holdsWhen(
			is.numeric(a) && length(a) == 1 &&
			!is.nan(a) &&
			is.null(names(a)),
			a != a
		) +

		run(5)

	})

message("worksWhen")

	over(val) +

	describe('identity should always work') +
	worksWhen(
		TRUE,
		identity(val)
	) +

	run()

message("worksWhen failures")

	assert_throws_error({

		over(str, val) +

		describe('adding a string always fails') +
		worksWhen(
			is.character(str),
			str + val,
			val + str
		) +

		run()

	})

message("failsWhen")

	over(str, val) +

	describe('adding a string always fails') +
	failsWhen(
		is.character(str),
		str + val,
		val + str
	) +

	run()


message("failsWhen failures")

	assert_throws_error({

		over(val) +

		describe('identity should always work') +
		failsWhen(
			TRUE,
			identity(val)
		) +

		run()

	})
