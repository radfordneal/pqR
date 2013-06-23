source("time.r")
 
test.name <- "pow.scalar"

b <- 0.7

f <- test.cmp(function(){ for (i in 1:3000000) a <- 2^b; a })
sys.time(a<-f())
print(a)

f <- test.cmp(function(){ for (i in 1:3000000) a <- b^2; a })
sys.time(a<-f())
print(a)


test.name <- "pow.vec-to-scalar"

x <- seq(1,2,length=100)
y <- seq(1,2,length=1000)
z <- seq(1,2,length=10000)

d2 <- 2
d1 <- 1
d0 <- 0
dm1 <- -1
d0.5 <- 0.5

f <- test.cmp(function(){ for (i in 1:3000) a <- z^0.3; a})
sys.time(a<-f())
print(c(sum(a),sum(a^2)))

f <- test.cmp(function(){ for (i in 1:3000) a <- z^d0.5; a})
sys.time(a<-f())
print(c(sum(a),sum(a^2)))

f <- test.cmp(function(){ for (i in 1:10000) a <- z^d1; a})
sys.time(a<-f())
print(c(sum(a),sum(a^2)))

f <- test.cmp(function(){ for (i in 1:10000) a <- z^d0; a})
sys.time(a<-f())
print(c(sum(a),sum(a^2)))

f <- test.cmp(function(){ for (i in 1:10000) a <- z^dm1; a})
sys.time(a<-f())
print(c(sum(a),sum(a^2)))

f <- test.cmp(function(){ for (i in 1:2000000) a <- x^2; a})
sys.time(a<-f())
print(c(sum(a),sum(a^2)))

f <- test.cmp(function(){ for (i in 1:1000000) a <- y^2; a})
sys.time(a<-f())
print(c(sum(a),sum(a^2)))

f <- test.cmp(function(){ for (i in 1:100000) a <- z^d2; a})
sys.time(a<-f())
print(c(sum(a),sum(a^2)))

f <- test.cmp(function(){ for (i in 1:100000) a <- z*z; a})
sys.time(a<-f())
print(c(sum(a),sum(a^2)))


test.name <- "pow.vec-to-vec"

m <- seq(1,2,length=100)
o <- rep(c(1,2),length=100)
two <- rep(2.0,100)
oi <- rep(1:7,length=100)
xi <- rep(1:11,length=100)

f <- test.cmp(function(){ for (i in 1:100000) a <- xi^oi; a})
sys.time(a<-f())
print(c(sum(a),sum(a^2)))

f <- test.cmp(function(){ for (i in 1:100000) a <- x^m; a})
sys.time(a<-f())
print(c(sum(a),sum(a^2)))

f <- test.cmp(function(){ for (i in 1:1000000) a <- x^o; a})
sys.time(a<-f())
print(c(sum(a),sum(a^2)))

f <- test.cmp(function(){ for (i in 1:2000000) a <- x^two; a})
sys.time(a<-f())
print(c(sum(a),sum(a^2)))
