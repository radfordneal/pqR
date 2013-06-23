source("time.r")
 
test.name <- "min-max.real"

ar1 <- seq(0.001,1,length=1000)
ar2 <- rev(ar1)
ar3 <- ar1*ar2

f <- test.cmp (function () {for (i in 1:500000) b <- max(ar3); b})
sys.time(b<-f()); print(b)

f <- test.cmp (function () {for (i in 1:50000) b <- pmax(ar1,ar2); b})
sys.time(b<-f()); print(b[c(1,2,999,1000)])

f <- test.cmp (function () {for (i in 1:500000) b <- min(ar3); b})
sys.time(b<-f()); print(b)

f <- test.cmp (function () {for (i in 1:50000) b <- pmin(ar1,ar2); b})
sys.time(b<-f()); print(b[c(1,2,999,1000)])

ar1 <- as.integer(100*ar1)
ar2 <- as.integer(100*ar2)
ar3 <- as.integer(100*ar3)

test.name <- "min-max.int"

f <- test.cmp (function () {for (i in 1:500000) b <- max(ar3); b})
sys.time(b<-f()); print(b)

f <- test.cmp (function () {for (i in 1:50000) b <- pmax(ar1,ar2); b})
sys.time(b<-f()); print(b[c(1,2,999,1000)])

f <- test.cmp (function () {for (i in 1:500000) b <- min(ar3); b})
sys.time(b<-f()); print(b)

f <- test.cmp (function () {for (i in 1:50000) b <- pmin(ar1,ar2); b})
sys.time(b<-f()); print(b[c(1,2,999,1000)])

test.name <- "min-max.string"

ar1 <- as.character(100+ar1)
ar2 <- as.character(100+ar2)
ar3 <- as.character(100+ar3)

f <- test.cmp (function () {for (i in 1:50000) b <- max(ar3); b})
sys.time(b<-f()); print(b)

f <- test.cmp (function () {for (i in 1:20000) b <- pmax(ar1,ar2); b})
sys.time(b<-f()); print(b[c(1,2,999,1000)])

f <- test.cmp (function () {for (i in 1:50000) b <- min(ar3); b})
sys.time(b<-f()); print(b)

f <- test.cmp (function () {for (i in 1:20000) b <- pmin(ar1,ar2); b})
sys.time(b<-f()); print(b[c(1,2,999,1000)])
