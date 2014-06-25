
forall <- kiwi:::forall
test_cases <- kiwi:::test_cases

require(kiwi)

message("xCompose (+)")

	over(val) +

	describe() +
	when(
		True,
		xCompose_(identity)(val) %is% val
	) +

	run()

message("xCompose (-)")

	over() +

	describe("fails with no functions") +
	failswhen(
		True,
		xCompose(list())
	) +

	run()
