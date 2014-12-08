list(
	one_hundred_keys  = rep('a', 100),
	one_hundred_vals  = 1:100,

	one_thousand_keys = rep('a', 1000),
	one_thousand_vals = 1:1000,

	ten_thousand_keys  = rep('a', 10000),
	ten_thousand_vals  = 1:10000
)




xAddKeys(one_hundred_keys, one_hundred_vals)
xAddKeys(one_thousand_keys, one_thousand_vals)
xAddKeys(ten_thousand_keys, ten_thousand_vals)
