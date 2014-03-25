
# 1. Select the largest row in a data-frame like struture
#    by an attribute.

GDP <- list(
	list('Saudi Arabia', 711050 * 10^6),
	list('Switzerland', 631050 * 10^6),
	list('Netherlands', 770067 * 10^6),
	list('Iran', 551588 * 10^6)
)

x_(GDP) $ x_MaxBy(xSecondOf)

# 2.

