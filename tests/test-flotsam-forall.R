
kiwi ::: load_test_dependencies(environment())

message("forall-next (+)")

	over(a, b) +
	describe("addition is commutative") +
	when(
		is.numeric(a)  && is.numeric(b) &&
		length(a) == 1 && length(b) == 1 &&
		is.finite(a)  && is.finite(b),
		a + b == b + a
	) +
	run()

	over(a) +
	describe("multiplication by 1 is identity") +
	when(
		is.numeric(a) && length(a) == 1,
		a * 1 == a
	) +
	run()

message("forall-next (-)")

	assert_throws_error <- function (expr) {

		res <- tryDefault(expr, True)

		if (!isTRUE(res)) {
			stop('error in test')
		}
	}


	assert_throws_error({

		over(a) +

		describe('a is not a') +
		when(
			is.numeric(a) && length(a) == 1 &&
			!is.nan(a),
			a != a
		) +

		run()

	})
