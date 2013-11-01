
message("xConst")

test_that("xConst", {

	expect_equal(xConst(10)(), 10)
	expect_equal(xConst(10)(10000), 10)

})
