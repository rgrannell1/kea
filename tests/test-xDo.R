
kiwi ::: load_test_dependencies(environment())

message("xDo")

	over(coll) +

	describe("xDo is always null") +
	holdsWhen(
		is_collection(coll),

		xDo(identity, coll)  %is% NULL
	) +

	run()

message("xDo")

	over(coll) +

	describe("coll must always be a collection") +
	failsWhen(
		!is_collection(coll),

		xDo(identity, coll)
	) +

	run()
