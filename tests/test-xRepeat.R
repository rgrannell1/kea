
kiwi ::: load_test_dependencies(environment())

message("xRepeat")

	over(num, coll) +

	describe("repeating a coll-0 times is length-zero") +
	holdsWhen(
		is_collection(coll),
		length(xRepeat(0, coll)) == 0
	) +

	describe("repeating an empty coll is length-zero") +
	holdsWhen(
		is.number(num) && round(num) == num && length(num) == 1 &&
		is_collection(coll) && length(coll) == 0,
		length(xRepeat(num, coll)) == 0
	) +

	describe("repeating num times is length(coll) x num") +
	holdsWhen(
		is.number(num) && round(num) == num && length(num) == 1 &&
		is_collection(coll),
		length(xRepeat(num, coll)) == length(coll) * num
	) +

	run()
