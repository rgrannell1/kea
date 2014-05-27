

arrow ::: load_test_dependencies(environment())

message("forall positive controls")

	over(a, b) +
	describe("addition is commutative") +
	when(
		is.numeric(a)  && is.numeric(b) &&
		length(a) == 0 && length(b) == 0 &&
		is.finite(a)   && is.finite(b),
		a + b == b + a
	) +
	run()
