# Question 1

cadmium <- c(162.1, 199.8, 220.0, 194.4, 204.3, 218.9, 153.7, 199.6, 210.7,
             179.0, 203.7, 236.1, 200.4, 278.2, 294.8, 341.1, 330.2, 344.2)
harvest <- c(rep(1, 6), rep(2, 6), rep(3, 6))
sludge <- rep(0:5, 3)

friedman.test(cadmium, sludge, harvest)

# Question 2

aflatoxin <- c(21, 29, 16, 20, 13, 5, 18, 26, 17, 4, 23, 30, 19, 19, 10, 12,
               18, 32, 20, 10, 15, 21, 18, 18, 14, 6, 12, 21, 9, 2)
spray <- c(rep("A", 10), rep("B", 10), rep("C", 10))
ear <- rep(1:10, 3)

friedman.test(aflatoxin, spray, ear)