set.seed (6)
x <- matrix ( rnorm (10 * 100), 10, 100)
x[, 1:50] <- x[, 1:50] + 0.5


t.test (x[, 1], mu = 0)$p.value

p.values <- rep (0, 100)
for (i in 1:100) {
p.values[i] <- t. test (x[, i], mu = 0)$p.value
}


decision <- rep ("Do not reject H0", 100)
decision[p.values <= .05] <- " Reject H0"

table (decision ,
       c( rep ("H0 is False ", 50), rep ("H0 is True ", 50))
)


m <- 1:500
fwe1 <- 1 - (1 - 0.05)^m
fwe2 <- 1 - (1 - 0.01)^m
fwe3 <- 1 - (1 - 0.001)^m

install.packages("ISLR2")
library (ISLR2)
fund.mini <- Fund[, 1:5]
t.test (fund.mini[, 1], mu = 0)

fund.pvalue <- rep (0, 5)
#for (i in 1:5)
fund.pvalue[0] <- t.test (fund.mini[, 0], mu = 0)$p.value
fund.pvalue
