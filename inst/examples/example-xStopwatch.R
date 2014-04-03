
# 1 Monte Carlo estimation of Pi
# estimate the value of pi in a preset amount of time.

probs_2d <- xPartial...(runif, 2)

in_unit_circle <- (point) := {
	sum(point^2) <= 1
}

# create the timer function.
time_left <- xStopwatch(2)

x_(logical(0)) $ xIterate(trials := {

	if (!time_left()) {
		Return(trials)
	}

	random_point <- probs_2d()
	c(trials, in_unit_circle(random_point))

}) $
xMap(trial := {
	if (trial) 1 else 0
}) $
x_Tap(unlist %then% mean) * 4
