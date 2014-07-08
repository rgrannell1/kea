
# 1. Create a simple csv parser.

from_csv <- xExplode('[ 	]*,[ 	]*')
from_csv("field1, field2, field3")

# c("field1, field2, field3")
