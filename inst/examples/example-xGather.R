
# 1. A toy example

xGather(c(0, 0, 0, 1, 0, 1, 1, 0))

# xGather_(0, 0, 1, 1, 1, 0, 1, 0)




# 2. Get the length of the longest stretch of consecutive one's in a binary
#    vector (an interview question my friend was asked).

bits <- c(0, 0, 1, 1, 0, 1, 1, 1, 1, 1, 0, 1, 0)

x_(bits) $ xGather() $ xMap(xLenOf) $ x_MaxBy(xI)

# 5
