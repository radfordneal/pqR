source("time.r")
 
a <- seq(1,3,length=1000)
b <- seq(2,4,length=1000)
as <- as.character(as.integer(100*a))
bs <- as.character(as.integer(100*b))
s <- seq(1,4,length=4)
n <- 200000

test.name <- "vec-bool.<="
f <- test.cmp (function () { for (i in 1:n) r <- 3<=a; r})
sys.time(r <- f()); print(sum(r))
f <- test.cmp (function () { for (i in 1:n) r <- a<=3; r})
sys.time(r <- f()); print(sum(r))
f <- test.cmp (function () { for (i in 1:n) r <- a<=b; r})
sys.time(r <- f()); print(sum(r))
f <- test.cmp (function () { for (i in 1:n) r <- a<=s; r})
sys.time(r <- f()); print(sum(r))
f <- test.cmp (function () { for (i in 1:(n/10)) r <- as<=bs; r})
sys.time(r <- f()); print(sum(as.integer(r)))

test.name <- "vec-bool.>="
f <- test.cmp (function () { for (i in 1:n) r <- 3>=a; r})
sys.time(r <- f()); print(sum(r))
f <- test.cmp (function () { for (i in 1:n) r <- a>=3; r})
sys.time(r <- f()); print(sum(r))
f <- test.cmp (function () { for (i in 1:n) r <- a>=b; r})
sys.time(r <- f()); print(sum(r))
f <- test.cmp (function () { for (i in 1:n) r <- a>=s; r})
sys.time(r <- f()); print(sum(r))
f <- test.cmp (function () { for (i in 1:(n/10)) r <- as>=bs; r})
sys.time(r <- f()); print(sum(as.integer(r)))

test.name <- "vec-bool.<"
f <- test.cmp (function () { for (i in 1:n) r <- 3<a; r})
sys.time(r <- f()); print(sum(r))
f <- test.cmp (function () { for (i in 1:n) r <- a<3; r})
sys.time(r <- f()); print(sum(r))
f <- test.cmp (function () { for (i in 1:n) r <- a<b; r})
sys.time(r <- f()); print(sum(r))
f <- test.cmp (function () { for (i in 1:n) r <- a<s; r})
sys.time(r <- f()); print(sum(r))
f <- test.cmp (function () { for (i in 1:(n/10)) r <- as<bs; r})
sys.time(r <- f()); print(sum(as.integer(r)))

test.name <- "vec-bool.>"
f <- test.cmp (function () { for (i in 1:n) r <- 3>a; r})
sys.time(r <- f()); print(sum(r))
f <- test.cmp (function () { for (i in 1:n) r <- a>3; r})
sys.time(r <- f()); print(sum(r))
f <- test.cmp (function () { for (i in 1:n) r <- a>b; r})
sys.time(r <- f()); print(sum(r))
f <- test.cmp (function () { for (i in 1:n) r <- a>s; r})
sys.time(r <- f()); print(sum(r))
f <- test.cmp (function () { for (i in 1:(n/10)) r <- as>bs; r})
sys.time(r <- f()); print(sum(as.integer(r)))

test.name <- "vec-bool.=="
f <- test.cmp (function () { for (i in 1:n) r <- 3==a; r})
sys.time(r <- f()); print(sum(r))
f <- test.cmp (function () { for (i in 1:n) r <- a==3; r})
sys.time(r <- f()); print(sum(r))
f <- test.cmp (function () { for (i in 1:n) r <- a==b; r})
sys.time(r <- f()); print(sum(r))
f <- test.cmp (function () { for (i in 1:n) r <- a==s; r})
sys.time(r <- f()); print(sum(r))
f <- test.cmp (function () { for (i in 1:(n/2)) r <- as==bs; r})
sys.time(r <- f()); print(sum(as.integer(r)))

test.name <- "vec-bool.!="
f <- test.cmp (function () { for (i in 1:n) r <- 3!=a; r})
sys.time(r <- f()); print(sum(r))
f <- test.cmp (function () { for (i in 1:n) r <- a!=3; r})
sys.time(r <- f()); print(sum(r))
f <- test.cmp (function () { for (i in 1:n) r <- a!=b; r})
sys.time(r <- f()); print(sum(r))
f <- test.cmp (function () { for (i in 1:n) r <- a!=s; r})
sys.time(r <- f()); print(sum(r))
f <- test.cmp (function () { for (i in 1:(n/2)) r <- as!=bs; r})
sys.time(r <- f()); print(sum(as.integer(r)))

u <- a>1.5
v <- b<3.2
w <- s==2
n <- 200000

test.name <- "vec-bool.&"
f <- test.cmp (function () { for (i in 1:n) r <- u&v; r})
sys.time(r <- f()); print(sum(r))
f <- test.cmp (function () { for (i in 1:n) r <- u&w; r})
sys.time(r <- f()); print(sum(r))
f <- test.cmp (function () { for (i in 1:n) r <- w&v; r})
sys.time(r <- f()); print(sum(r))

test.name <- "vec-bool.|"
f <- test.cmp (function () { for (i in 1:n) r <- u|v; r})
sys.time(r <- f()); print(sum(r))
f <- test.cmp (function () { for (i in 1:n) r <- u|w; r})
sys.time(r <- f()); print(sum(r))
f <- test.cmp (function () { for (i in 1:n) r <- w|v; r})
sys.time(r <- f()); print(sum(r))
