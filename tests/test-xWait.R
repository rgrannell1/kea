
message("xWait")

test_that("xWait", {

	expect_that(xWait(identity, 2)(1), equals(1))
	time <- system.time( xWait(identity, 1)(1) )[3]
	expect_true(time > 0.9 && time < 1.1)

})
