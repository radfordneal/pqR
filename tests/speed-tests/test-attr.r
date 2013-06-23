source("time.r")
 
a <- 9
attributes(a) <- list (dd=1.4, cc=1.3, bb=1.2, aa=1.1)

test.name <- "attr"

n <- 2000000

f <- test.cmp (function () {for (i in 1:n) r <- attr(a,"bb"); r })

sys.time(r<-f())
print(r)

f <- test.cmp (function () {for (i in 1:n) r <- attr(a,"b"); r })

sys.time(r<-f())
print(r)

n <- 1000000

f <- test.cmp (function () {for (i in 1:n) { b <- a; attr(b,"bb") <- NULL }; b})

sys.time(r<-f())
print(r)

n <- 1000000

f <- test.cmp (function () {for (i in 1:n) { b <- a; attr(b,"bb") <- "x" }; b})

sys.time(r<-f())
print(r)
