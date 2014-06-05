
arrow ::: load_test_dependencies(environment())
is_collection <- arrow ::: is_collection

message("xSelect (+)")

	over(coll) +
	describe("the empty collection always yields the list") +
	when(
		length(coll) == 0 && is_collection(coll),
		xSelect(function (x) True, coll)  %equals% list(),
		xSelect(function (x) False, coll) %equals% list(),
		xSelect(function (x) Na, coll)    %equals% list()
	) +

	over(coll) +
	describe("truth function acts as identity") +
	when(
		length(coll) > 0 && is_collection(coll),
		xSelect(function (x) True, coll) %equals% as.list(coll)
	) +

	over(coll) +
	describe("false or na function acts as unit") +
	when(
		length(coll) > 0 && is_collection(coll),
		xSelect(function (x) False, coll) %equals% list(),
		xSelect(function (x) Na,    coll) %equals% list()
	) +
	run()

message("xSelect (-)")

#	over(fn, coll) +
#	describe("coll must always be a collection") +
#	failsWhen(
#		!is_collection(coll),
#		xSelect(identity, coll)
#	) +
#	run()
