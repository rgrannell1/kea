
# 1. xByCols is the canonical way to work with
#    data frames in Arrow.

toxicity <- x_(data.frame(
	"toxicant" = 
		c("melamine", "caffeine", "nicotine"),
	"ld50 (mg/kg)" =
		c(6800, 192, 50)
))

toxicity $ x_ByCols()

# list(
#     toxicant       = c("melamine", "caffeine", "nicotine"),
#     "ld50 (mg/kg)" = c(6800, 192, 50)
# )
