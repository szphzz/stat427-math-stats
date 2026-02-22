# Question 1

1 - pbinom(65, 75, 0.8)

# Question 2

high <- c(32.9, 33.2, 32.0, 33.1, 33.5, 34.6, 32.1, 33.1, 30.2, 29.1)
low <- c(31.8, 31.9, 29.2, 33.2, 33.0, 33.9, 31.0, 32.5, 28.9, 28.0)
diff <- high - low
diff

## Part a

minus <- sum(diff < 0)
pbinom(minus, length(diff), 0.5)

## Part c

6 + 8.5 + 10 + 2 + 4 + 6 + 3 + 8.5 + 6

wilcox.test(high, low, paired=TRUE, alternative="greater")

# Question 3

estimate <- c(79, 75, 82, 83, 69, 65, 49, 62, 77, 67, 66, 59, 80, 57, 86, 55, 80, 
              74, 77, 63)
actual <- rep(60, 20)
diff <- estimate - actual

## Part a

plus <- sum(diff > 0)
1 - pbinom(15, 20, 0.5) + pbinom(4, 20, 0.5)

## Part b

wilcox.test(estimate, alternative="two.sided", mu=60)

## Part c

t.test(estimate, alternative="two.sided", mu=60)

# Question 4

A <- c(6.1, 9.2, 8.7, 8.9, 7.6, 7.1, 9.5, 8.3, 9.0)
B <- c(9.1, 8.2, 8.6, 6.9, 7.5, 7.9, 8.3, 7.8, 8.9)

## Part a

wilcox.test(A, B, alternative="two.sided")

## Part b

t.test(A, B, alternative="two.sided", var.equal=TRUE)

# Question 5

A <- c(84, 128, 168, 92, 184, 92, 76, 104, 72, 180, 144, 120)
B <- c(140, 184, 368, 96, 480, 188, 480, 244, 440, 380, 480, 196)

wilcox.test(A, B, alternative="less")

# Question 6

response <- c(0.33, 0.29, 0.21, 0.32, 0.25, 0.28, 0.41, 0.34, 0.39, 0.27, 0.21, 
              0.30, 0.26, 0.33, 0.31)
campaign <- c(rep(1, 5), rep(2, 5), rep(3, 5))

## Part b

kruskal.test(response, campaign)

## Part c

two <- response[campaign == 2] 
three <- response[campaign == 3]

wilcox.test(two, three, alternative="greater")

# Question 7

damage <- c(10.8, 15.6, 19.2, 17.9, 18.3, 9.8, 16.7, 19.0, 20.3, 19.4, 22.3, 
            19.5, 18.6, 24.3, 19.9, 20.4, 23.6, 21.2, 19.8, 22.6, 9.8, 12.3, 
            16.2, 14.1, 15.3, 10.8, 12.2, 17.3, 15.1, 11.3)
chemical <- c(rep("A", 10), rep("B", 10), rep("C", 10))

kruskal.test(damage, chemical)
