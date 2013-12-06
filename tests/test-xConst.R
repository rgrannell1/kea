
message("xConst")

forall(
	"const of a value is a function",
	test_cases$collection,
	xConst(coll)() %equals% coll
)
