list(
	one_hundred  = rep(TRUE, 100),
	one_thousand = rep(TRUE, 1000),
	ten_thousand = rep(TRUE, 10000)
)

xAllOf(truth, one_hundred)
xAllOf(truth, one_thousand)
xAllOf(truth, ten_thousand)
