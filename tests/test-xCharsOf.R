
kea ::: load_test_dependencies(environment())

message("xCharsOf")

	over(str) +

	it("charsof is an integer") +
	holdsWhen(
		is_character(str) && length(str) == 1 && !is.na(str),

		is.integer(xCharsOf(str)),
		length(xCharsOf) == 1
	) +

	it("charsof is an integer") +
	holdsWhen(
		is_character(str) && length(str) == 1 && !is.na(str),

		xCharsOf(str) %is% nchar(unlist(str))
	) +

	run()
