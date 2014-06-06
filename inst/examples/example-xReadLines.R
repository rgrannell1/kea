
# 1. Read the first line of kiwi's description.

x_(system.file(package = 'kiwi', 'DESCRIPTION')) $
xReadLines() $
x_Take(1)

# list("Package: kiwi")
