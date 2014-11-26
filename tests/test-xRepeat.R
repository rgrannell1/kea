
kea ::: load_test_dependencies(environment())

message("xRepeat")

	over(num, coll) +

	it("repeating a coll-0 times is length-zero") +
	holdsWhen(
		suchThat $ is_collection(coll),

		length(xRepeat(0, coll)) == 0
	) +

	it("repeating an empty coll is length-zero") +
	holdsWhen(
		is_numeric(num) && length(num) == 1 && !is.na(unlist(num)) &&
		round(unlist(num)) == num && is.finite(unlist(num)) &&
		num > 0 && num < 1000 &&
		suchThat $ is_empty_collection(coll),

		length(xRepeat(num, coll)) == 0
	) +

	it("repeating num times is length(coll) x num") +
	holdsWhen(
		is_numeric(num) && length(num) == 1 && !is.na(unlist(num)) &&
		round(unlist(num)) == num && is.finite(unlist(num)) &&
		num > 0 && num < 1000 &&
		suchThat $ is_collection(coll),

		length(xRepeat(num, coll)) == length(coll) * num
	) +

	it("repeating num times preserves & repeats names") +
	holdsWhen(
		is_numeric(num) && length(num) == 1 && !is.na(unlist(num)) &&
		round(unlist(num)) == num && is.finite(unlist(num)) &&
		num > 0 && num < 1000 &&
		suchThat $ is_collection(coll),

		names(xRepeat(num, coll)) %is% rep(names(coll), num)
	) +

	run()
