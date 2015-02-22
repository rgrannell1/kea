
# 1. order a vector by another.

names   <- c('joe', 'jack', 'jay')
ages    <- c(15, 32, 14)
heights <- c(185, 180, 167)

names[xOrderOf(heights / ages)]

# c("jack", "jay", "joe")
