
message('xSucc')

forall(
	"the successor of an empty collection is an empty double",
	test_cases$collection_zero,
	xSucc(coll) %equals% as.double(coll)
)

forall(
	"the successor is doubles",
	test_cases$integers,
	xSucc(coll) %equals% as.double(coll + 1)
)

forall(
	"infinity acts acts an identity",
	test_cases$infinity,
	xSucc(coll) %equals% as.double(coll)
)

message('arrow $ xSucc')

forall(
	"arrow.xSucc is doubles",
	test_cases$infinity,
	x_(coll)$xSucc()$x() %equals% as.double(coll)
)

message('arrow $ xSucc...')

forall(
	"arrow.xSucc... is doubles",
	test_cases$integers,
	x_(coll)$xApply(xSucc...)$x() %equals% as.double(coll + 1)
)
