
message("xTimer")

test_that("xTimer", {

  	timer <- xTimer(1)
	expect_that(timer(), equals(True))
	Sys.sleep(1)
	expect_that(timer(), equals(False))

})
