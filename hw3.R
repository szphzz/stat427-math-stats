# Question 1

x <- c(103.768, 92.295, 100.615, 102.754,
      88.602, 61.675, 88.017, 108.579,
      73.003, 90.677, 71.210, 73.154,
      123.086, 84.023, 82.115, 106.755,
      91.052, 76.014, 89.222, 90.479)
mu0 <- 100
xbar <- mean(x)
s <- sqrt(var(x))
n <- length(x)
t <- (xbar - mu0)/(s/sqrt(n)) # test statistic
t

pt(t, df=n-1) # p-value

t.test(x, alternative="less", mu=100) # check

# Question 2

## Part a

mu0 <- 10
sigma <- 1.88
n <- 15
mu <- 9

d <- (mu - mu0)/sigma
ncp <- sqrt(n)*d

pt(qt(0.05, df=n-1), df=n-1, ncp=ncp) # power

## Part b

mu <- 8 # update
d <- (mu - mu0)/sigma
ncp <- sqrt(n)*d

pt(qt(0.05, df=n-1), df=n-1, ncp=ncp)

## Part c

install.packages("pwr")
library(pwr)

mu0 <- 10
sigma <- 1.88
mu <- 9

d <- (mu - mu0)/sigma
pwr.t.test(d=d, power=0.9, type="one.sample", alternative="less")

# Question 3

## Part b

d <- 0.75
alpha <- 0.1
n <- 28
ncp <- sqrt(n)*d
stat <- qt(alpha/2, df=n-1)

pt(stat, df=n-1, ncp=ncp) + 1 - pt(-stat, df=n-1, ncp=ncp)

## Part d

d <- -0.75
ncp <- sqrt(n)*d

pt(stat, df=n-1, ncp=ncp) + 1 - pt(-stat, df=n-1, ncp=ncp)

# Question 4

## Part a

library(ggplot2)

calculate_power <- function(d, n, alpha) {
  stat <- qt(alpha/2, df=n-1)
  ncp <- sqrt(n)*d
  power <- pt(stat, df=n-1, ncp=ncp) + 1 - pt(-stat, df=n-1, ncp=ncp)
  return (power)
}

d <- seq(-2, 2, by=0.1)
n <- c(5, 10, 50)

power_data <- expand.grid(d=d, n=n)
power_data$power <- sapply(1:nrow(power_data), function(i) calculate_power(power_data$d[i], 
                                                                           power_data$n[i],
                                                                           0.05))

ggplot(power_data, aes(x=d, y=power, color=factor(n))) +
  geom_line() +
  scale_color_discrete(name = "Sample Size (n)") +
  labs(x="Cohen's d", y="Power", title="Power Function for Different Sample Sizes, alpha=0.05")

## Part c

alpha <- c(0.01, 0.05, 0.1)

power_data <- expand.grid(d=d, alpha=alpha)
power_data$power <- sapply(1:nrow(power_data), function(i) calculate_power(power_data$d[i], 
                                                                           20,
                                                                           power_data$alpha[i]))

ggplot(power_data, aes(x=d, y=power, color=factor(alpha))) +
  geom_line() +
  scale_color_discrete(name="Alpha") +
  labs(x="Cohen's d", y="Power", title="Power Function for Different Significance Levels, n=20")

# Question 5

## Part a

xbar1 <- 0.78
xbar2 <- 1.26
Sp <- 0.32
n1 <- 7
n2 <- 7
D0 <- 0

Tp <- (xbar1 - xbar2 - D0)/(Sp*sqrt(1/n1+1/n2))
pt(Tp, df=n1+n2-2)

## Part c
calculate_power <- function(diff, sigma, n1, n2, alpha) {
  ncp <- diff/(sigma*sqrt(1/n1 + 1/n2))
  stat <- qt(alpha, df=n1+n2-2)
  power <- pt(stat, df=n1+n2-2, ncp=ncp)
  return (power)
}

diff <- seq(-1, 1, by=0.1)
n1 <- c(7, 4)
n2 <- c(7, 10)

power_data <- expand.grid(diff=diff, n1=n1, n2=n2)
power_data <- power_data[power_data$n1 == 7 & power_data$n2 == 7 | power_data$n1 == 4 & power_data$n2 == 10,]
power_data$power <- sapply(1:nrow(power_data), function(i) calculate_power(power_data$diff[i], 
                                                                           0.32,
                                                                           power_data$n1[i],
                                                                           power_data$n2[i],
                                                                           0.05))

ggplot(power_data, aes(x=diff, y=power, color=factor(n1))) +
  geom_line() +
  scale_color_discrete(name="n1") +
  labs(x="mu1 - mu2", y="Power", title="Power Function for Different Sample Sizes")
