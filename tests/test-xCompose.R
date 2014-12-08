
kea ::: load_test_dependencies(environment())

require(kea)

message("xCompose")

	over(val) +

	it("compsing of identity is identity") +
	holdsWhen(
		True,

		xCompose_(identity)(val) %is% val
	) +

	run()

message("xCompose")

	over() +

	it("fails with no functions") +
	failsWhen(
		True,

		xCompose(list())
	) +

	run()
