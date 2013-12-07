
forall <- arrow:::forall
test_cases <- arrow:::test_cases

named_list_zero <- list(a = 1)[0]

message("xAssoc")

forall(
	"associating the empty list is the named empty list",
	test_cases$recursive_zero,
	xAssoc(coll) %equals% named_list_zero
)

message("arrow $ xAssoc")

forall(
	"collection.xAssoc works",
	x_(coll)$xAssoc()$x() %equals% False
)

message("arrow $ xAssoc...")
