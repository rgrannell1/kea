
forall <- arrow:::forall
test_cases <- arrow:::test_cases

require(arrow)

message("xRestOf")

	forall(
		"xRestOf of an empty collection always yields the empty list.",
		test_cases$collection,
		xRestOf(coll) %equals% list(),
		given =
			length(coll) == 0
	)

	forall(
		"xRestOf of a list shortens the list by one (usually)",
		test_cases$collection,
		length(xRestOf(coll)) == length(coll) - 1,
		given =
			length(coll) > 0
	)
