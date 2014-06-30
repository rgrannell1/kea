
kiwi ::: load_test_dependencies(environment())


require(kiwi)

message("xCompose")

	over(val) +

	describe("compsing of identity is identity") +
	holdsWhen(
		True,
		xCompose_(identity)(val) %is% val
	) +

	run()

message("xCompose")

	over() +

	describe("fails with no functions") +
	failsWhen(
		True,
		xCompose(list())
	) +

	run()
