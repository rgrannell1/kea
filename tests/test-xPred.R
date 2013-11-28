
message('xPred')

forall(
	"the predeccesor of an empty collection is an empty double",
	test_cases$collection_zero,
	xPred(coll) %equals% as.double(coll)
)

forall(
	"the predeccesor is doubles",
	test_cases$integers,
	xPred(coll) %equals% as.double(coll - 1)
)

forall(
	"infinity acts as an identity",
	test_cases$infinity,
	xPred(coll) %equals% as.double(coll)
)

message('arrow $ xPred')

forall(
	"arrow.xPred is doubles",
	test_cases$infinity,
	x_(coll)$xPred()$x() %equals% as.double(coll)
)

message('arrow $ xPred...')

forall(
	"arrow.xPred... is doubles",
	test_cases$integers,
	x_(coll)$xApply(xPred...)$x() %equals% as.double(coll - 1)
)
