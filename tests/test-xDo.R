
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

	over(fn, coll) +

	describe("coll must always be a collection") +
	failsWhen(
		!is_collection(coll),
		xDo(identity, coll)
	) +

	describe("fn must always be a function") +
	failsWhen(
		!is.function(fn),
		xDo(fn, list())
	) +

	run()
