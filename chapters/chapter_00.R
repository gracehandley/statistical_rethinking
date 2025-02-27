print('All models are wrong, but some are useful.')

x <- 1:2
x <- x*10
x <- log(x)
x <- sum(x)
x <- exp(x)
x

(log(.01^200)) # rounds to 0 so returns -Inf
(200*log(.01)) # does not round so returns correct answer (-921.034)
