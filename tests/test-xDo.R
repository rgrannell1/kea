
kiwi ::: load_test_dependencies(environment())
is_collection <- kiwi ::: is_collection

message("xDo (+)")

	over(coll) +

	describe("xDo is always null") +
	when(
		is_collection(coll),
		xDo(identity, coll)  %equals% NULL
	) +

	run()

message("xDo (-)")

	over(coll) +

	describe("coll must always be a collection") +
	failsWhen(
		!is_collection(coll),
		xDo(identity, coll)
	) +

	run()
