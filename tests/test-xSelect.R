
forall <- arrow:::forall
test_cases <- arrow:::test_cases

arrow ::: load_test_dependencies(environment())

require(arrow)

message("xSelect")

	over(coll) +
	describe("the empty collection always yields the list") +
	when(
		length(coll) == 0,
		xSelect(function (x) True, coll)  %equals% list(),
		xSelect(function (x) False, coll) %equals% list(),
		xSelect(function (x) Na, coll)    %equals% list()) +
	run()

	over(coll) +
	describe("truth function acts as identity") +
	when(
		length(coll) > 0,
		xSelect(function (x) True, coll) %equals% as.list(coll)
	) +
	run()

	over(coll) +
	describe("false or na function acts as unit") +
	when(
		length(coll) > 0,
		xSelect(function (x) False, coll) %equals% list(),
		xSelect(function (x) Na, coll) %equals% list()
	) +
	run()
