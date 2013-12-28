
#1. print the files in your current directory.

xDo(print, list.files('.'))

#    or

x_(list.files('.'))$xDo(print)

#    or even

xDo(print %of% list.files, '.')

#2. plot a trig function.

plot(
    0, 0,
    xlim = c(0, 10),
    ylim = c(-10, 10),
    type = 'n')

xDo(
    x := {

        y <- tan( sin( x ) / cos( x )^2 )
        points(x, y)

    },
    (1:1000) / 100
)
