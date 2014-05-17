
# 1. Shuffling can allow you to take a random sample from a dataset
# that may or may not be ordered.

x_(runif(100)) $ xShuffle() $ xTake(30) $ x_Reduce('+') / 30

# ~ 0.5
