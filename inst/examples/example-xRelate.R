
# 1. use xRelate when you want to keep the value and its map around.

# BAD

xMap_(fn_name := list(fn_name, get(fn_name)), 'Map', 'lapply')

# list(list('Map', <fn body>), list('lapply', <fn body>))

# GOOD

xRelate_(get, 'Map', 'lapply')

# list(list('Map', <fn body>), list('lapply', <fn body>))
