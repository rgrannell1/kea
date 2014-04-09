
approx_log <- local({

	makePolynomial <- xAsUnary((a : b : c: d: e) := {
		function (x, reflect = False) {

			if (reflect) return(c(a, b, c, d, e))

			a*x^4 + b*x^3 + c*x^2 + d *x + e
		}
	})

	fitness <- local({

		xs <- seq(0.001, 100, by = 0.01)

		makePolynomial %then% (fn := {
			x_(abs(fn(xs) - log(xs)) / log(xs)) $ x_Reduce("+")
		})
	})

	mutate <- local({

		indices_sets <- xPowerSetOf(1:5)

		coeffs := {

			indices <- xOneOf(indices_sets)

			x_(coeffs) $ x_MapIndexed((x : ith) := {

				if (ith %in% indices) {
					x * rnorm(1, 0, 3)
				} else {
					x
				}
			})

		}
	})

	xIterate(
		state := {

			best <- state $ best

			if (state $ iter == 3000) {
				state $ best <- makePolynomial(best)
				Return(state)
			}

			if (state $ iter %% 10 == 0) {
				print(state $ iter)
			}

			population <- x_(best) $ xRepeat(5) $ xChunk(5) $ xMap(mutate) $ xJoin...(list(best))

			state $ best <- population $ x_MinBy(fitness)

			state $ scores <- c(state $ scores, fitness(state $ best))

			state $ iter <- state $ iter + 1

			state
		},
		list(
			iter = 0,
			best = list(1, 1, 1, 1, 1),
			scores = numeric(0))
	)
})

require(ggplot2)

plot(log10(approx_log$scores), type = 'l')

approx_log $ best(10)

plot(log(1:1000))
lines(approx_log $ best(1:1000), col = 'red')

