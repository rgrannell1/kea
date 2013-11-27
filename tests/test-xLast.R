
message('xLast')

forall(
	"first always returns the last element of a collection",
	test_cases$collection,
	xLast(coll) %equals% coll[[length(coll)]],
	given =
		length(coll) <= 1
)

message('arrow $ xLast')

forall(
	"collection $ xLast",
	test_cases$collection,
	x_(coll)$xLast() %equals% coll[[length(coll)]],
	given =
		length(coll) <= 1
)
