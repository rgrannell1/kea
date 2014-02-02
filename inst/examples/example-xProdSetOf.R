
# 1. xProdSetOf with xMap, xMapply or xReduce are
#    generally very nice replacements for loops.

xMapply(
    (ith : jth) := {
        ith^2 + jth^2
    },
    xProdSetOf...(1:4, 1:4)
)

# is roughly the same as base R's...

res <- list()

for (ith in 1:4) {
    for (jth in 1:4) {
        res <- c(res, ith^2 + jth^2)
    }
}

res

# 2. Compute the standard childhood multiplication
#    table.

xProdSetOf...(1:12, 1:12)
