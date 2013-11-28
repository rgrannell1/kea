
message("xRecurMap")

test_that("xRecurMap", {

	expect_equal(xRecurMap(identity, list()), list())
	expect_equal(
		xRecurMap( identity, list(1, list(2, 3)) ),
		list(1, list(2, 3)) )
	expect_equal(
		xRecurMap( identity, list(1, list(2, 3)) ),
		list(1, list(2, 3)) )
	expect_equal(
		xRecurMap( paste0, list(1, list(2, 3)) ),
		list('1', list('2', '3')) )

})
