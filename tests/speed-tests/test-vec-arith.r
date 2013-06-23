source("time.r")
 
a <- seq(1,2,length=1000)
b <- seq(3,4,length=1000)
s <- seq(5,6,length=4)
n <- 500000

test.name <- "vec-arith.+"
f <- test.cmp (function () { for (i in 1:n) r <- 1+a; r})
sys.time(r <- f()); print(sum(r))
f <- test.cmp (function () { for (i in 1:n) r <- a+1; r})
sys.time(r <- f()); print(sum(r))
f <- test.cmp (function () { for (i in 1:n) r <- a+b; r})
sys.time(r <- f()); print(sum(r))
f <- test.cmp (function () { for (i in 1:n) r <- a+s; r})
sys.time(r <- f()); print(sum(r))
f <- test.cmp (function () { for (i in 1:n) r <- s+a; r})
sys.time(r <- f()); print(sum(r))

test.name <- "vec-arith.-"
f <- test.cmp (function () { for (i in 1:n) r <- 1-a; r})
sys.time(r <- f()); print(sum(r))
f <- test.cmp (function () { for (i in 1:n) r <- a-1; r})
sys.time(r <- f()); print(sum(r))
f <- test.cmp (function () { for (i in 1:n) r <- a-b; r})
sys.time(r <- f()); print(sum(r))
f <- test.cmp (function () { for (i in 1:n) r <- a-s; r})
sys.time(r <- f()); print(sum(r))
f <- test.cmp (function () { for (i in 1:n) r <- s-a; r})
sys.time(r <- f()); print(sum(r))

test.name <- "vec-arith.*"
f <- test.cmp (function () { for (i in 1:n) r <- 3*a; r})
sys.time(r <- f()); print(sum(r))
f <- test.cmp (function () { for (i in 1:n) r <- a*3; r})
sys.time(r <- f()); print(sum(r))
f <- test.cmp (function () { for (i in 1:n) r <- a*b; r})
sys.time(r <- f()); print(sum(r))
f <- test.cmp (function () { for (i in 1:n) r <- a*s; r})
sys.time(r <- f()); print(sum(r))
f <- test.cmp (function () { for (i in 1:n) r <- s*a; r})
sys.time(r <- f()); print(sum(r))

test.name <- "vec-arith./"
f <- test.cmp (function () { for (i in 1:n) r <- 3/a; r})
sys.time(r <- f()); print(sum(r))
f <- test.cmp (function () { for (i in 1:n) r <- a/3; r})
sys.time(r <- f()); print(sum(r))
f <- test.cmp (function () { for (i in 1:n) r <- a/b; r})
sys.time(r <- f()); print(sum(r))
f <- test.cmp (function () { for (i in 1:n) r <- a/s; r})
sys.time(r <- f()); print(sum(r))
f <- test.cmp (function () { for (i in 1:n) r <- s/a; r})
sys.time(r <- f()); print(sum(r))
