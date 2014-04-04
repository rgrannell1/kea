
# 1. How many lines are there in "The Twelve Days of Christmas"?
#    [lyrics from Wikipedia]
#
#    it is a cumulative song, with each new verse adding an extra line
#
#    V1. (1 line)
#    On the First day of Christmas my true love sent to me
#    a Partridge in a Pear Tree.
#
#    V2. (2 lines)
#    On the Second day of Christmas my true love sent to me
#    Two Turtle Doves
#    and a Partridge in a Pear Tree.
#
#    V3. (3 lines)
#    On the Third day of Christmas my true love sent to me
#    Three French Hens,
#    Two Turtle Doves
#    and a Partridge in a Pear Tree.
#
#    The song continues for twelve verses.
#
#    How many lines are sung in total?
#
#   scan lets you see the cumulative number of lines sung by the end of each verse.
#
#   total lines = 1 + 2 + 3 + ... + 12
#
#   scan generates the sequence 0, 1, (1 + 2), (1 + 2 + 3), ...., (1 + 2 + ... + 12)

xScan("+", 0, 1:12)

# list(0, 1, 3, 6, 10, 15, 21, 28, 36, 45, 55, 66, 78)
