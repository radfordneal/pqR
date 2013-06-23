source("time.r")
 
test.name <- "complex-expr"

f <- test.cmp (function (n,a,b) 
{ for (i in 1:n) r <- exp (sin(1-abs(a*b)+5*b) / (1+cos(a+b+1/b+1)) - 1)
  r
})

sys.time(r <- f(1000000,1.1,2.2))
print(r)

u <- seq(1,2,length=2000)
v <- seq(2,3,length=2000)
sys.time(r <- f(10000,u,v))
print(sum(r))

g <- test.cmp (function (n,a,b,c) 
{ for (i in 1:n) r <- a + ((- b + 3L*a - c + 30000L) %/% (a+100L)) %% 5L
  r
})

sys.time(r <- g(1000000,300L,500L,700L))
print(r)

u <- 1:100
v <- 101:200
w <- 1001:1100
sys.time(r <- g(100000,u,v,w))
print(sum(r))
