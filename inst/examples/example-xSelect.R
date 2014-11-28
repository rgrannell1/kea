
# 1. CodeEval Given a number and a collection of numbers,
# find all unique pairs in the collection that sum to that number.

coll <- c(3,-3, 1, 2, 9, -12, 3, 8, 4, 5, 1)
expected_sum <- 8

x_(xProdSetOf_(coll, coll))               $
xSelect(
	xApply(`+`) %then% xIs(expected_sum)) $
x_DistinctOf()
