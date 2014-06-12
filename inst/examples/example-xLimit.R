
# 1. Get random words from an API.

# require(RCurl)

# url <- "http://randomword.setgetgo.com/get.php"

# random_word <-
#	x_( function (...) paste(httpGET(url)) ) $
#	x_Limit(3)


# xMap(random_word, 1:4)

# list("regulative", "pulverizator", "unsigmatic", NULL)

# the final request isn't executed, since the limit
# of three calls was exceeded.
