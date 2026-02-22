# Question 1

a <- c(5, 2, 2)
b <- c(2, 2, 5)

curve(dbeta(x, a[1], b[1]), 0, 2,
      col="green", lty=1, xlab="x", ylab="Density",
      main="Part c: Skewness")
curve(dbeta(x, a[2], b[2]), col="orange", lty=2, add=T)
curve(dbeta(x, a[3], b[3]), col="blue", lty=3, add=T)
legend("topright",legend=c("Left-skewed", "Symmetric", "Right-skewed"), lty=c(1,2,3),
       col=c("green","orange", "blue"))

# Question 2

## Part b

r = 3
n = 10
sum = 55
curve(dbeta(x, 2+n*r, 6+sum-n*r), 0, 1,
      col="red", lty=2, xlab="p", ylab="Density",
      main="Part b: sum=55")
curve(dbeta(x, 2, 6), col="blue", add=T)
legend("topright",legend=c("Prior", "Posterior"), lty=c(1,2),
       col=c("blue","red"))

## Part c

sum = 173
curve(dbeta(x, 2+n*r, 6+sum-n*r), 0, 1,
      col="red", lty=2, xlab="p", ylab="Density",
      main="Part c: sum=173")
curve(dbeta(x, 2, 6), col="blue", add=T)
legend("topright",legend=c("Prior", "Posterior"), lty=c(1,2),
       col=c("blue","red"))

# Question 4

## Part a

curve(dgamma(x, 1, 1), 0, 10,
      col="green", lty=1, xlab=expression(mu), ylab="Density",
      main="Part a: Comparing priors")
curve(dgamma(x, 4, 4), col="orange", lty=2, add=T)
curve(dgamma(x, 16, 4), col="blue", lty=3, add=T)
legend("topright",legend=c("Prior 1", "Prior 2", "Prior 3"), lty=c(1,2,3),
       col=c("green","orange", "blue"))

gamma_mean <- function(alpha, lambda) {
  return (alpha/lambda)
}

gamma_sd <- function(alpha, lambda) {
  return (sqrt(alpha/lambda^2))
}

gamma_mean(1, 1)
gamma_sd(1, 1)

gamma_mean(4, 4)
gamma_sd(4, 4)

gamma_mean(16, 4)
gamma_sd(16, 4)


## Part b

x <- c(2, 1, 1, 0, 0, 1, 2, 0, 0, 1)

sum <- sum(x)
n <- length(x)

curve(dgamma(x, 1+sum, 1+n), 0, 4, ylim=c(0,2),
      col="green", lty=1, xlab=expression(mu), ylab="Density",
      main="Part b: Comparing posteriors")
curve(dgamma(x, 4+sum, 4+n), col="orange", lty=2, add=T)
curve(dgamma(x, 16+sum, 4+n), col="blue", lty=3, add=T)
legend("topright",legend=c("Prior 1", "Prior 2", "Prior 3"), lty=c(1,2,3),
       col=c("green","orange", "blue"))

## Part c

gamma_mean(1+sum, 1+n)
gamma_mean(4+sum, 4+n)
gamma_mean(16+sum, 4+n)

## Part d

qgamma(c(0.05, 0.95), 1+sum, 1+n)
qgamma(c(0.05, 0.95), 4+sum, 4+n)
qgamma(c(0.05, 0.95), 16+sum, 4+n)

