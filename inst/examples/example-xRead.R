
# 1. Read the first line of kea's description.

x_(system.file(package = 'kea', 'DESCRIPTION')) $ xRead() $ xToLines() $ x_At(1)

#  "Package: kea"
