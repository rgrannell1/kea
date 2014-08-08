
# 1. xByRows is the canonical way to work with
#    data frames in Kea.

toxicity <- x_(data.frame(
	"toxicant" =
		c("melamine", "caffeine", "nicotine"),
	"ld50 (mg/kg)" =
		c(6800, 192, 50)
))

toxicity $ x_ByRows()

#
# list(
#     list(toxicant = "melamine", ld50 (mg/kg) = 6800)
#     list(toxicant = "caffeine", ld50 (mg/kg) = 192)
#     list(toxicant = "nicotine", ld50 (mg/kg) = 50)
# )
#