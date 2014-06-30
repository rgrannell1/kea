
kiwi ::: load_test_dependencies(environment())

message("forall-next (+)")

	over(a, b) +

	describe("addition is commutative") +
	holdsWhen(
		is.numeric(a)  && is.numeric(b) &&
		length(a) == 1 && length(b) == 1 &&
		is.finite(a)  && is.finite(b) &&
		is.null(names(a)) && is.null(names(b)) &&
		a < 100000 && b < 100000,
		a + b == b + a
	) +

	run(5)

	over(a) +

	describe("multiplication by 1 is identity") +
	holdsWhen(
		is.numeric(a) && length(a) == 1 &&
		is.finite(a) &&
		is.null(names(a)),
		a * 1 == a
	) +

	run(5)

message("forall-next (-)")

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
