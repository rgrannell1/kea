
arrow ::: load_test_dependencies(environment())
is_collection <- arrow ::: is_collection

message("xReject (+)")

	over(coll) +

	describe("the empty collection always yields the list") +
	when(
		length(coll) == 0 && is_collection(coll),
		xReject(function (x) True, coll)  %equals% list(),
		xReject(function (x) False, coll) %equals% list(),
		xReject(function (x) Na, coll)    %equals% list()
	) +

	describe("truth function acts as identity") +
	when(
		length(coll) > 0 && is_collection(coll),
		xReject(function (x) True, coll) %equals% list()
	) +

	describe("false or na function acts as unit") +
	when(
		length(coll) > 0 && is_collection(coll),
		xReject(function (x) False, coll) %equals% as.list(coll),
		xReject(function (x) Na,    coll) %equals% as.list(coll)
	) +
	run()

message("xReject (-)")

#	over(fn, coll) +
#	describe("coll must always be a collection") +
#	failsWhen(
#		!is_collection(coll),
#		xReject(identity, coll)
#	) +
#	run()
