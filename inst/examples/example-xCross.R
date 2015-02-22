
# 1. xCross with xMap or xReduce is
#    often a very nice replacements for looping.

xMap(
    xApply((ith : jth) := {
        ith^2 + jth^2
    }),
    xCross_(1:4, 1:4)
)

# is roughly the same as base R's_

res <- list()

for (ith in 1:4) {
    for (jth in 1:4) {
        res <- c(res, ith^2 + jth^2)
    }
}

res

# 2. Compute the standard childhood multiplication
#    table.

xCross_(1:12, 1:12)
