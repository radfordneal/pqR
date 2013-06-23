source("time.r")
 
# Assess degree of parallelism using helper threads with simple subscripting.


test.name <- "hlp-sub.one"

f <- test.cmp (function (n)
{ for (k in 1:n)
  { u <- colSums(M1)
    s <- 0
    for (i in 1:length(u)) if (u[i]>2) s <- tanh(s^4) + exp(-s^3/7) + sin(s)^3/5
  }
  s
})

N <- 2000
M <- 200
M1 <- matrix(c(1,2),N,M)

options(helpers_disable=TRUE)
sys.time (s<-f(2000))
print(s)

options(helpers_disable=FALSE)
sys.time (s<-f(2000))
print(s)


test.name <- "hlp-sub.two"

g <- test.cmp (function (n)
{ for (k in 1:n)
  { P <- M1 %*% M2
    s <- 0
    for (i in 1:ncol(P)) if (P[2,i]>6) s <- s + sin(sqrt(exp(tanh(abs(s^3)))))/7
  }
  s
})

N <- 1000
M <- 500

M1 <- matrix(c(1,2),2,N)
M2 <- matrix(c(3.1,3.2),N,M)

options(helpers_disable=TRUE)
sys.time (s<-g(1000))
print(s)

options(helpers_disable=FALSE)
sys.time (s<-g(1000))
print(s)
