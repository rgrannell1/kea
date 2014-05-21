
# 1. Read the first line of arrow's description.

x_(system.file(package = 'arrow', 'DESCRIPTION')) $
xReadLines() $
x_Take(1)

# list("Package: arrow")
