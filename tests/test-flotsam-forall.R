
arrow ::: load_test_dependencies(environment())

message("forall positive controls")

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
		is.integer(a) && length(a) == 1,
		a * 1 == a
	) +
	run()

