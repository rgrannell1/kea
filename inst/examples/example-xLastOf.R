
# 1. extract a filename from a full path-name

my_path <- '/home/ryan/Code/kea.R/xMap.R'

x_(my_path) $ xExplode('/') $ x_LastOf()

# xMap.R
