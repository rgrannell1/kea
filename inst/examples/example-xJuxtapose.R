
# 1. Collect several summary statistics of a dataset
#    simultaneously.

nums <- xList[c(a, a^2, a^3), a <- 1:10]

x_(nums) $ x_Map(xJuxtapose_(max, mean, min))
