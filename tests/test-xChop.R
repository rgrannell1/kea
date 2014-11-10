
kea ::: load_test_dependencies(environment())

message("xChop")

	over(num, coll) +

	describe("xChopping infinite times / length times is as.list") +
	holdsWhen(
		is_collection(coll) && length(coll) > 0,
		length(xChop(Inf,          coll)) == length(coll),

		length(xChop(length(coll), coll)) == length(coll)
	) +

	describe("xChop once is almost identity") +
	holdsWhen(
		is_collection(coll) && length(coll) > 0,

		xChop(1, coll) %is% list(as.list(coll))
	) +

	describe("xChop always returns the correct number of elements") +
	holdsWhen(
		is.numeric(num) && !is.na(unlist(num)) &&
		(length(num) == 1 && round(unlist(num)) == num & num >= 0) &&
		is_collection(coll),

		length(xChop(num, coll)) == min(length(coll), num)
	) +

	run()
