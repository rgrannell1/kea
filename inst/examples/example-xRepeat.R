
# 1. Use xRepeat to repeat a sine wave.

range <- seq(0, 2*pi, len = 600)
wave <- xRepeat(5, sin(range))

plot(unlist(wave), type = 'l')
