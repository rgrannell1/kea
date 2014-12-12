
list(
	fn1 = function (x1) x1,
	fn2 = function (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10) x1
)

xFix_(fn1, x1 = 1)
xFix (fn1, list(x1 = 1))

xFix_(fn2, x1 = 1, x2 = 2, x3 = 3, x4 = 4, x5 = 5, x6 = 6, x7 = 7, x8 = 8, x9 = 9, x10 = 10)
xFix (fn2, list(x1 = 1, x2 = 2, x3 = 3, x4 = 4, x5 = 5, x6 = 6, x7 = 7, x8 = 8, x9 = 9, x10 = 10))
