
# 1. Read the first 100 letters of kiwi's description.

x_(system.file(package = 'kiwi', 'DESCRIPTION')) $
xReadChars() $
x_Take(100)

# list("P", "a", "c", "k", "a", "g", "e", ":", " ", "a")
