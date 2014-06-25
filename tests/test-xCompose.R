
kiwi ::: load_test_dependencies(environment())
is_collection <- kiwi ::: is_collection

require(kiwi)

message("xCompose (+)")

	over(val) +

	describe() +
	holdsWhen(
		True,
		xCompose_(identity)(val) %is% val
	) +

	run()

message("xCompose (-)")

	over() +

	describe("fails with no functions") +
	failsWhen(
		True,
		xCompose(list())
	) +

	run()
