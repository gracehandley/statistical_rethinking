# Statistical Rethinking 2nd edition by Richard McElreath
# Chapter 2: R exercises
# Author: Grace Handley
# Date created: 2025-03-03
# Date updated: 2025-03-29

library(rethinking)

ways <- c(0, 3, 8, 9, 0)
ways/sum(ways)

# Bayesian design loop:
# 1) Data story: narrative
# 2) Update: train the model
# 3) Evaluate: revise the model

dbinom(6, size = 9, prob = 0.5)

##### Grid approximation #####
# Set up 20-point grid
p_grid <- seq(from = 0, to = 1, length.out = 20)
prior <- rep(1, 20)
likelihood <- dbinom(6, size = 9, prob = p_grid)
unstd.posterior <- likelihood * prior
posterior <- unstd.posterior / sum(unstd.posterior)

plot(p_grid, posterior, type = "b",
     xlab = "Probability of water",
     ylab = "Posterior probability")
mtext("20 points")

# Sparser grid (5 points)
p_grid_sparse <- seq(from = 0, to = 1, length.out = 5)
prior_sparse <- rep(1, 5)
likelihood_sparse <- dbinom(6, size = 9, prob = p_grid_sparse)
unstd.posterior_sparse <- likelihood_sparse * prior_sparse
posterior_sparse <- unstd.posterior_sparse / sum(unstd.posterior_sparse)

plot(p_grid_sparse, posterior_sparse, type = "b",
     xlab = "Probability of water",
     ylab = "Posterior probability")
mtext("5 points")

# Denser grid (1000 points)
p_grid_dense <- seq(from = 0, to = 1, length.out = 1000)
prior_dense <- rep(1, 1000)
likelihood_dense <- dbinom(6, size = 9, prob = p_grid_dense)
unstd.posterior_dense <- likelihood_dense * prior_dense
posterior_dense <- unstd.posterior_dense / sum(unstd.posterior_dense)

plot(p_grid_dense, posterior_dense, type = "b",
     xlab = "Probability of water",
     ylab = "Posterior probability")
mtext("1000 points")

# Change the prior for the 20-point grid
prior_new <- ifelse(p_grid < 0.5, 0, 1)
prior_new <- exp(-5 * abs(p_grid - 0.5))

unstd.posterior_new <- likelihood * prior_new
posterior_new <- unstd.posterior_new / sum(unstd.posterior_new)

plot(p_grid, posterior_new, type = "b",
     xlab = "Probability of water",
     ylab = "Posterior probability")
mtext("20 points, updated prior")

##### Quadratic (Gaussian) approximation #####
globe.qa <- quap(
  alist(
    W ~ dbinom(W + L, p), # binomial likelihood
    p ~ dunif(0, 1) # uniform prior
  ),
  data = list(W = 6, L = 3))

precis(globe.qa) # "Assuming the posterior is Gaussian, it is maximized at 0.67 and its standard deviation is 0.16"

# Analytical calculation
W <- 6
L <- 3
curve(dbeta(x, W + 1, L + 1), from = 0, to = 1)
curve(dnorm(x, 0.67, 0.16), lty = 2, add = TRUE) # quadratic approximation

##### Markov chain Monte Carlo (MCMC) #####
n_samples <- 1000
p <- rep(NA, n_samples)
p[1] <- 0.5
W <- 6
L <- 3
for (i in 2:n_samples) {
  p_new <- rnorm(1, p[i - 1], 0.1)
  if(p_new < 0) p_new <- abs(p_new)
  if(p_new > 1) p_new <- 2 - p_new
  q0 <- dbinom(W, W + L, p[i-1])
  q1 <- dbinom(W, W + L, p_new)
  p[i] <- ifelse(runif(1) < q1/q0, p_new, p[i - 1])
}

dens(p, xlim = c(0,1))
curve(dbeta(x, W + 1, L + 1), lty = 2, add = TRUE)

##### Practice #####
# 2E1 - 2) Pr(rain|Monday) and 4) Pr(rain, Monday) / Pr(Monday)
# 2E2 - 3) The probability of Monday, given that it is raining
# 2E3 - 1) Pr(Monday|rain) and 4) Pr(rain | Monday) Pr(Monday) / Pr(rain)
# 2E4 - It means that given our observations, we think the probability of water is 0.7, but this is based on incomplete knowledge. There is a true physical proportion of the globe covered in water, but we are inferring it based on limited knowledge.

# 2M1-1 W, W, W (3 W in 3) grid approximation
p_grid <- seq(from = 0, to = 1, length.out = 100)
prior <- rep(1, 100)
likelihood <- dbinom(3, size = 3, prob = p_grid)
unstd.posterior <- likelihood * prior
posterior <- unstd.posterior / sum(unstd.posterior)

plot(p_grid, posterior, type = "b",
     xlab = "Probability of water",
     ylab = "Posterior probability")
mtext("2M1-1 W, W, W")
plot(posterior ~ p_grid, type = "l") # alt plot

# 2M1-2 W, W, W, L (3 W in 4)
likelihood <- dbinom(3, size = 4, prob = p_grid)
unstd.posterior <- likelihood * prior
posterior <- unstd.posterior / sum(unstd.posterior)

plot(p_grid, posterior, type = "b",
     xlab = "Probability of water",
     ylab = "Posterior probability")
mtext("2M1-2 W, W, W, L")
plot(posterior ~ p_grid, type = "l") # alt plot

# 2M1-3 L, W, W, L, W, W, W (5 W in 7)
likelihood <- dbinom(5, size = 7, prob = p_grid)
unstd.posterior <- likelihood * prior
posterior <- unstd.posterior / sum(unstd.posterior)

plot(p_grid, posterior, type = "b",
     xlab = "Probability of water",
     ylab = "Posterior probability")
mtext("2M1-3 L, W, W, L, W, W, W")
plot(posterior ~ p_grid, type = "l") # alt plot

# 2M2-1
p_grid <- seq(from = 0, to = 1, length.out = 100)
prior <- ifelse(p_grid < 0.5, 0, 1)
likelihood <- dbinom(3, size = 3, prob = p_grid)
unstd.posterior <- likelihood * prior
posterior <- unstd.posterior / sum(unstd.posterior)

plot(p_grid, posterior, type = "b",
     xlab = "Probability of water",
     ylab = "Posterior probability")
mtext("2M2-1 W, W, W")

# 2M2-2
likelihood <- dbinom(3, size = 4, prob = p_grid)
unstd.posterior <- likelihood * prior
posterior <- unstd.posterior / sum(unstd.posterior)

plot(p_grid, posterior, type = "b",
     xlab = "Probability of water",
     ylab = "Posterior probability")
mtext("2M2-2 W, W, W, L")

# 2M2-3
likelihood <- dbinom(5, size = 7, prob = p_grid)
unstd.posterior <- likelihood * prior
posterior <- unstd.posterior / sum(unstd.posterior)

plot(p_grid, posterior, type = "b",
     xlab = "Probability of water",
     ylab = "Posterior probability")
mtext("2M1-3 L, W, W, L, W, W, W")

# 2M3
# Pr(land | Earth) = 1 - 0.7 = 0.3
# Pr(land | Mars) = 1
# Pr(Earth) = 0.5 and Pr(Mars) = 0.5
# Pr(Earth | land) = Pr(land | Earth) Pr(Earth) / Pr(land)
  # Pr(Earth | land) = Pr(land | Earth) Pr(Earth) / Pr(land | Earth) Pr(Earth) + Pr(land | Mars) Pr(Mars)
  # Pr(Earth | land) = 0.3 * 0.5 / (0.3 * 0.5 + 1 * 0.5)
0.3 * 0.5 / (0.3 * 0.5 + 1 * 0.5)
# [1] 0.2307692

# 2M4
# Card 1 = B/B, card 2 = B/W, card 3 = W/W
# 3 ways to draw B; 2 from B/B and 1 from B/W
# 2 out of 3 ways may result in the other side of the card being B

# 2M5
# Add a 4th card that is also B/B
# 5 ways to draw B; 4 from B/B and 1 from B/W
# 4 out of 5 ways may result in the other side of the card being B

# 2M6
# 1 B/B -> 2 B/W and 3 W/W
# 1 x 2 ways for B/B to produce black side up
# 2 x 1 ways for B/W to produce black side up
# (1 x 2) + (2 x 1) = 4 ways to see black side up; 2 out of 4 come from B/B

# 2M7
# Sequence of B followed by W
# If you drew B/B, you have 3 ways to get 2nd card to show W. 2 cards * 3 = 6 ways to show B then W
# If you drew B/W, you can only get W by drawing the W/W card. 1 card * 2 ways = 2 ways to show B then W
# 6 + 2 = 8 ways to produce B then W sequence
  # 6 ways arise from drawing B/B card first
  # Therefore 6/8 probability the reverse of your 1st card is B (= 0.75)

# 2H1
# Identical panda species, each .5 of population
  # A births twins .1 and singletons .9
  # B births twins .2 and singletons .8
# New female panda just gave birth to twins. What is probability of next birth being twins?

# Pr(twins_2 | twins_1) = Pr(twins_1, twins_2) / Pr(twins)
  # Pr(twins) = 0.5(0.1) + 0.5(0.2) = 0.15
  # Pr(twins_1, twins_2) = 0.5(0.1 * 0.1) + 0.5(0.2 * 0.2) = 0.025
# Pr(twins_2 | twins_1) = 0.025 / 0.15 = 0.17

# 2H2
# Probability of A given you've seen 1 birth of twins

# Pr(A | twins_1) = Pr(twins_1 | A) Pr(A) / Pr(twins)
  # Pr(twins) = 0.5(0.1) + 0.5(0.2) = 0.15
  # Pr(twins 1 | A) = 0.1
  # Pr(A) = 0.5
# Pr(A | twins_1) = 0.1 * 0.5 / 0.15 = .33

# 2H3
# Probability of A given you've seen twins followed by a singleton
# Assume Pr(A | twins_1) from 2H2 = .33
# Now need to figure out Pr(A | singleton)

# Pr(A | singleton) = Pr(singleton | A) Pr(A) / Pr(singleton)
  # Pr(singleton | A) = 1 - 0.1 = 0.9
  # Pr(A) = .33
  # Pr(singleton) = (.33 * 0.9) + (.66 * 0.8) = .83
# Pr(A | singleton) = 0.9 * .33 / .83 = .36

# 2H4
# New test correctly identifies A .8 and correctly identifies B .65
# New panda tests positive as A

# Pr(test A | A) = 0.8
# Pr(test A | B) = 1 - 0.65 = 0.35

# Pr(A | test A) = Pr(test A | A) Pr(A) / Pr(test A | A) Pr(A) + Pr(test A | B) Pr(A)
  # Pr(A | test A) = 0.8 / (0.8 * .5) + (.35 * .5) = 0.7
# With the test, we increase confidence in A from 0.5 to 0.7

# Incorporate birth data...
# Pr(A | test A) = Pr(test A | A) Pr(A) / Pr(test A)
  # Pr(test A | A) = 0.8
  # Pr(A) = .36 # from 2H3 above
  # Pr(test A) = Pr(A) * Pr(test A | A) + Pr(B) * Pr(test A | B)
# Pr(A | test A) = (.8)(.36) / (.36)(.8) + (1 - .36)(.35) = .56
