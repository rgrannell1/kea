
# 1. call a variadic function with xApply

nums <- c(1, 5,2,3)

xApply(sum, nums)

# 11

# normally, variadic functions have to be called with
# a fixed number of arguments, as in

a = 1; b = 5; c = 2; d = 3

sum(a, b, c, d)

# 11

# which cannot be changed at runtime.

# 2. Reduce and xApply can often be used to solve the
#    same problems.

xReduce('+', 1:10)

# 55

xApply(sum, 1:10)

# 55

# 3. Rbind and xApply can be used to build up a matrix
#    from vectors.

xApply_(rbind, list(1, 2), list(3, 4), list(5, 6))

# structure(
#     list(1, 3, 5, 2, 4, 6),
#     .Dim = c(3L, 2L))
