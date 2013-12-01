
message("xLift")

test_that("xLift", {

	expect_equal(xLift("+", identity, identity)(10), 20)

})
