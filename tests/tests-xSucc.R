
message('xSucc')

test_that('xSucc', {

	expect_equal( xSucc(1), 2 )
	expect_equal( xSucc(c(1, 0)), c(2, 1) )
	expect_equal( xSucc(list(1, 2)), c(2, 3) )
	expect_equal( xSucc(Inf), Inf )
	expect_equal( xSucc(-Inf), -Inf )

})

forall(
	"the successor of an empty typed vector is an empty typed vector",
	list(coll = G$integers()),
	xSucc(coll) %equals% as.double(coll)
)
