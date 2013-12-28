
# 1. the archetypal example of Reduce; summing a vector.

xReduce("+", 1:10)

55

# 2. another associative operator; using rbind to join several
#    vectors into one matrix.

xReduce...(
    rbind,
    c(1, 2),
    c(3, 4))
)

matrix(1:4, 2, 2)
