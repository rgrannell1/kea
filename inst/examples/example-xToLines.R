
# 1. Lexically sort a list.
#

info <-
"
a: a for aardvark.
z: z for zebra.
d: d for deer.
f: f for ferret.
g: g for green snake.
y: y for yapok.
"

x_(info) $ xToLines() $ x_Tap(sort)
