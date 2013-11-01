
message("xLocatel")

test_that('xLocatel', {

	isEven <- function (n) {
		n %% 2 == 0
	}

	expect_equal(xLocatel(Truth, list()), integer(0))
	expect_equal(xLocatel(Falsity, 1:10), integer(0))
	expect_equal(xLocatel(Truth, 1:10), 1)
	expect_equal(xLocatel(isEven, 1:10), 2)

})
