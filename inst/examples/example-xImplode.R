
# 1. create a simple csv writer.

to_csv <- xImplode(', ')
to_csv(xTake(10, letters))

x_(letters) $ xImplode(',') $ x_Take(10)

# 'a,b,c,d,e,f,g,h,i,j'
