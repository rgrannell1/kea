
# 1. select prime numbers

primes <- upper := {

	is_prime <- num := {
	    if (num == 2) True else min(num %% 2:(num - 1)) > 0
	}

	xList[num, num <- 1:upper, is_prime(num)]
}

primes(17)

# list(2, 3, 5, 7, 11, 13, 17, 19)

# 2. xList has normal lexical scoping

first_name <- "Jack"
xList[paste(first_name, middleinit, sirname), middleinit <- c('K', 'M'), sirname <- c("Black", "Brown", "Gray")]

# list("Jack K Black", "Jack M Black",
#      "Jack K Brown", "Jack M Brown",
#      "Jack K Gray", "Jack M Gray")


# 3.
# Using list comprehensions to process tabular data.

raw_health_ranking <-
"
1     France             4
2     Italy             11
3     San-Marino        21
4     Andorra           23
5     Malta             37
6     Singapore         38
7     Spain             24
8     Oman              62
9     Austria            6
10    Japan             13
11    Norway            16
12    Portugal          27
13    Monaco            12
14    Greece            30
15    Iceland           14
16    Luxembourg         5
17    Netherlands        9
18    United-Kingdom    26
"

health_ranking <-
	x_(raw_health_ranking) $ xToLines() $ x_Map(xToWords)

top_twenty_countries <-
	xList[ tolower( row[[2]] ), row <- health_ranking ]

# list(
#	"france", "italy", "san-marino", "andorra",
#	"malta", "singapore", "spain", "oman", "austria",
#	"japan", "norway", "portugal", "monaco",
#	"greece", "iceland", "luxembourg", "netherlands",
#	"united-kingdom"
# )
