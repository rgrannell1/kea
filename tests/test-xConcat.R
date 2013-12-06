
message("xConcat")

forall(
	"a single collection acts as identity",
	test_cases$collection,
	xConcat(list(coll)) %equals% list(coll)
)

forall(
	"a single collection and null acts as identity",
	test_cases$collection,
	xConcat(list(coll, Null)) %equals% list(coll)
)