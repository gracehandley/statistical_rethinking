# Statistical Rethinking 2nd edition by Richard McElreath
# Preface: R exercises
# Author: Grace Handley
# Date created: 2025-02-27
# Date updated: 2025-02-27

print('All models are wrong, but some are useful.')

x <- 1:2
x <- x*10
x <- log(x)
x <- sum(x)
x <- exp(x)
x

(log(.01^200)) # rounds to 0 so returns -Inf
(200*log(.01)) # does not round so returns correct answer (-921.034)

# Load the data:
# car braking distances in feet paired with speeds in km/h
# see ?cars for details
data(cars)

# fit a linear regression of distance on speed
m <- lm(dist ~ speed, data = cars)

# estimated coefficients from the model
coef(m)

# plot residuals against speed
plot(resid(m) ~ speed, data = cars)

# Installing the rethinking R package
# Step 1: install CmdStanR from mc-stan.org/install
# Step 2: install.packages(c("coda","mvtnorm","devtools","loo","dagitty","shape"))
# library(devtools)
# devtools::install_github("rmcelreath/rethinking")