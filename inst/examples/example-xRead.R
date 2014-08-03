
# 1. Read the first line of kea's description.

x_(system.file(package = 'kea', 'DESCRIPTION')) $ xRead() $ xToLines() $ x_Take(1)

# list("Package: kea")
