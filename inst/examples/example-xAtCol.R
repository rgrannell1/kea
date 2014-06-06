
# 1. Selecting columns in data-frame like structures
#    Kiwi eshews data frames, but works well with lists of rows.
#    All

df <- xList[ list(ith, letters[ith], LETTERS[ith]), ith <- 1:26 ]

# [
#   [1, a, A]
#   [2, b, A]
#      .
#      .
#   [26, z, Z]
# ]

x_(df) $ x_AtCol(2)

# list("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l",
#      "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x",
#      "y", "z")
