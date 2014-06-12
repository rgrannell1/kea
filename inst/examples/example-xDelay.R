
# 1. Get random words from an API, slowly.
#
# There are two approaches you can take to throttling
# requests below a rate limit; slowing down the rate
# you make requests, or limit the number of requests.

# require(RCurl)

# url <- "http://randomword.setgetgo.com/get.php"

# random_word <-
#	x_( function (...) paste(httpGET(url)) ) $
#	xDelay(3)


# xMap(random_word, 1:4)

# list("regulative", "pulverizator", "unsigmatic", NULL)

# the final request isn't executed, since the limit
# of three calls was exceeded.
