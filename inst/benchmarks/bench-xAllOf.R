
list(
	one_hundred  = rep(TRUE, 100),
	one_thousand = rep(TRUE, 1000),
	ten_thousand = rep(TRUE, 10000)
)

xAllOf(identity, one_hundred)
xAllOf(identity, one_thousand)
xAllOf(identity, ten_thousand)
