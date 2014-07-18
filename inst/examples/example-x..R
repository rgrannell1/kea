
raw_rain_data <- "
August      1.9
September   1.5
October     1.3
November    0.9
December    0.6
January     0.7
February    0.8
March       1.3
April       1.4
May         2.1
June        2.6
July        2.9
"

# convert the data set to a list of named lists,
# giving the month and rain in millilitres.

rain_data <-
	x_(raw_rain_data) $ xToLines() $
	xMap(xToWords %then% as.list)  $
	xMap( xAddKeys(c('month', 'rain (ml)')) ) $
	x_Map(row := {

		row[[2]] <- as.numeric(xSecondOf(row))
		row
	})

# 1. Select the rain data column.

xMap(x. $ `rain (ml)`, rain_data)

# list(
#     1.9, 1.5, 1.3,
#     0.9, 0.6, 0.7,
#     0.8, 1.3, 1.4,
#     2.1, 2.6, 2.9
# )

# 2. Convert the column to proportions of
# the largest value.


# you could also use x_(rain_data) $ xAtCol(2) $ x_MaxBy(xI)

max_rainfall <- x_(rain_data) $ xMap(x. $ `rain (ml)`) $ x_MaxBy(xI)


prop_rain_data <- x_(rain_data) $ x_Map(row := {
	row[[2]] <- round(row[[2]] / max_rainfall, 1)
	row
})

# list(
#    list(month = "August",     `rain (ml)` = 0.7)
#    list(month = "September",  `rain (ml)` = 0.5)
#    list(month = "October",    `rain (ml)` = 0.4)
#    list(month = "November",   `rain (ml)` = 0.3)
#    list(month = "December", 	`rain (ml)` = 0.2)
#    list(month = "January", 	`rain (ml)` = 0.2)
#    list(month = "February", 	`rain (ml)` = 0.3)
#    list(month = "March",      `rain (ml)` = 0.4)
#    list(month = "April",      `rain (ml)` = 0.5)
#    list(month =  "May",       `rain (ml)` = 0.7)
#    list(month = "June",       `rain (ml)` = 0.9)
#    list(month = "July",       `rain (ml)` = 1.0)
# )

# 3. modulo 6 every element of an atomic vector.

xMap(x. %% 6, 0:11)
