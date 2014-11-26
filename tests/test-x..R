
kea ::: load_test_dependencies(environment())

message('Kea Wildcard Lambda\'s')

	over(num0, num1) +

	it("binary %%") +
	holdsWhen(
		is.numeric(num0) && length(num0) == 1 &&
		is.numeric(num1) && length(num1) == 1 &&
		!anyNA(num0) && !anyNA(num1) &&
		num0 != 0,

		(x. %% num1)(num0) %is% (num0 %% num1)
	) +

	it("binary %/%") +
	holdsWhen(
		is.numeric(num0) && length(num0) == 1 &&
		is.numeric(num1) && length(num1) == 1 &&
		!anyNA(num0) && !anyNA(num1) &&
		num0 != 0,

		(x. %/% num1)(num0) %is% (num0 %/% num1)
	) +

	it("binary ^") +
	holdsWhen(
		is.numeric(num0) && length(num0) == 1 &&
		is.numeric(num1) && length(num1) == 1 &&
		!anyNA(num0) && !anyNA(num1) &&
		num0 != 0,

		(x. ^ num1)(num0) %is% (num0 ^ num1)
	) +

	it("binary **") +
	holdsWhen(
		is.numeric(num0) && length(num0) == 1 &&
		is.numeric(num1) && length(num1) == 1 &&
		!anyNA(num0) && !anyNA(num1) &&
		num0 != 0,

		(x. ** num1)(num0) %is% (num0 ** num1)
	) +

	it("binary *") +
	holdsWhen(
		is.numeric(num0) && length(num0) == 1 &&
		is.numeric(num1) && length(num1) == 1 &&
		!anyNA(num0) && !anyNA(num1) &&
		num0 != 0,

		(x. * num1)(num0) %is% (num0 * num1)
	) +

	it("binary /") +
	holdsWhen(
		is.numeric(num0) && length(num0) == 1 &&
		is.numeric(num1) && length(num1) == 1 &&
		!anyNA(num0) && !anyNA(num1) &&
		num0 != 0,

		(x. / num1)(num0) %is% (num0 / num1)
	) +

	it("binary +") +
	holdsWhen(
		is.numeric(num0) && length(num0) == 1 &&
		is.numeric(num1) && length(num1) == 1 &&
		!anyNA(num0) && !anyNA(num1),

		(x. + num1)(num0) %is% (num0 + num1)
	) +

	it("binary -") +
	holdsWhen(
		is.numeric(num0) && length(num0) == 1 &&
		is.numeric(num1) && length(num1) == 1 &&
		!anyNA(num0) && !anyNA(num1),

		(x. - num1)(num0) %is% (num0 - num1)
	) +

	run()

	over(num) +

	it("unary -") +
	holdsWhen(
		is.numeric(num) && !anyNA(num),

		(-x.)(num) %is% -num
	) +

	it("unary +") +
	holdsWhen(
		is.numeric(num) && !anyNA(num),

		(+x.)(num) %is% +num
	) +

	run()

	over(bool) +

	it("unary !") +
	holdsWhen(
		is.logical(bool) && length(bool) == 1,

		(!x.)(bool) %is% !bool
	) +

	run()

