
kea ::: load_test_dependencies(environment())

message('xToChars')

	over(str) +

	it("works of empty string is empty vector") +
	holdsWhen(
		True,

		xToChars('') %is% character(0)
	) +

	it("length == nchar") +
	holdsWhen(
		is.character(str) && length(str) == 1 && !is.na(str),

		length(xToChars(str)) == nchar(str)
	) +

	run()
