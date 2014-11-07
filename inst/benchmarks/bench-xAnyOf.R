list(
	one_hundred  = rep(FALSE, 100),
	one_thousand = rep(FALSE, 1000),
	ten_thousand = rep(FALSE, 10000)
)

xAnyOf(truth, one_hundred)
xAnyOf(truth, one_thousand)
xAnyOf(truth, ten_thousand)
