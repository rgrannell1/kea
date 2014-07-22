
kiwi ::: load_test_dependencies(environment())

message("xIndicesTo")

	over(num) +

	describe("the empty collection always yields the empty integer vector") +
	holdsWhen(
		True,
		xIndicesTo(0)  %is% integer(0),
		xIndicesTo(0L) %is% integer(0)
	) +

	describe("the length is num") +
	holdsWhen(
		is.numeric(num) && length(num) == 1 && !is.na(num) &&
		round(num) == num && num > 0 && is.finite(num) &&
		num < 100000,
		length(xIndicesTo(num)) == num
	) +


	describe("the upper index is the length of the collection") +
	holdsWhen(
		is.numeric(num) && length(num) == 1 && !is.na(num) &&
		round(num) == num && num > 0 && is.finite(num) &&
		num < 100000,
		xIndicesTo(num) %is% seq_len(num)
	) +

	run()
