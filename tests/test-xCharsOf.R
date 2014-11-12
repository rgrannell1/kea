
kea ::: load_test_dependencies()

message("xCharsOf")

	over(str) +

	describe("charsof is an integer") +
	holdsWhen(
		is_character(str) && length(str) == 1 && !is.na(str),

		is.integer(xCharsOf(str)),
		length(xCharsOf) == 1
	) +

	describe("charsof is an integer") +
	holdsWhen(
		is_character(str) && length(str) == 1 && !is.na(str),

		xCharsOf(str) %is% nchar(unlist(str))
	) +

	run()
